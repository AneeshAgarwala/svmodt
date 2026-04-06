#' @title Predict Using a Support Vector Machine Oblique Decision Tree
#' @description
#' Predicts class labels or class probabilities for new data using a tree
#' constructed with SVM splits. Handles leaf nodes, internal nodes, recursive traversal,
#' and fallback mechanisms when SVM predictions or scaling fail.
#'
#' @param tree A tree node object (leaf or internal) created by \code{svm_split} or \code{svm_split_enhanced}.
#' @param newdata A data frame of new predictor values. **Must contain the same features** as those used to fit the tree.
#'   Any additional columns (including responses) are ignored.
#' @param return_probs Logical; if \code{TRUE}, returns both predicted class labels and class probabilities.
#' @param calibrate_probs Logical; if \code{TRUE}, converts SVM decision values to probabilities using logistic
#'   calibration (sigmoid) based on the distance from the hyperplane. If \code{FALSE}, fallback probabilities
#'   are computed from class frequencies at the leaf node.
#'
#' @return
#' If \code{return_probs = FALSE}, a character vector of predicted class labels.
#' If \code{return_probs = TRUE}, a list with elements:
#' \itemize{
#'   \item \code{predictions}: Character vector of predicted class labels.
#'   \item \code{probabilities}: Numeric matrix of class probabilities
#'     (rows = samples, columns = classes).
#' }
#'
#' @details
#' The function traverses the SVM-based oblique decision tree recursively and predicts class labels or probabilities. Key behaviors:
#' \itemize{
#'   \item \strong{Leaf nodes:} Return the majority class stored in the node, along with class probabilities.
#'   \item \strong{Internal nodes:}
#'     \itemize{
#'       \item Scale features according to the node's scaling parameters.
#'       \item Compute SVM decision values.
#'       \item Recursively traverse left and right children depending on the sign of the decision value.
#'     }
#'   \item \strong{Binary support:}
#'     \itemize{
#'       \item Binary SVMs produce a single decision value per node.
#'     }
#'   \item \strong{Fallback predictions:} If scaling fails, SVM predictions are unavailable, or child nodes are missing, predictions are generated in this order:
#'     \itemize{
#'       \item SVM-provided probabilities (if available).
#'       \item Calibrated decision values using a logistic/sigmoid function (if \code{calibrate_probs = TRUE}).
#'       \item Leaf node class distribution (empirical frequencies) or uniform probabilities as a last resort.
#'     }
#'   \item \strong{Probability normalization:} All returned probabilities are normalized so that each row sums to 1.
#'   \item \strong{Feature requirement:} \code{newdata} must contain exactly the features used to train the tree; any extra columns, including responses, are ignored.
#'   \item \strong{Calibration behavior:}
#'     \itemize{
#'       \item \code{calibrate_probs = FALSE} returns class frequencies at the leaf node.
#'       \item \code{calibrate_probs = TRUE} uses the distance from the hyperplane for logistic post-processing into probabilities.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Train DTSVM tree
#' tree <- svm_split_enhanced(
#'   data = wdbc,
#'   response = "diagnosis",
#'   max_depth = 3,
#'   max_features = 2,
#'   feature_method = "cor",
#'   class_weights = "balanced_subsample"
#' )
#'
#' # Predict on WDBC data
#' preds <- predict_svm_tree(tree, newdata = wdbc)
#'
#' # Predict with probabilities and logistic calibration
#' result <- predict_svm_tree(tree, newdata = wdbc,
#'                            return_probs = TRUE, calibrate_probs = TRUE)
#' }
#' @export
svm_predict_tree <- function(tree, newdata, return_probs = FALSE,
                             calibrate_probs = TRUE) {
  # Handle leaf nodes
  if (tree$is_leaf) {
    pred <- rep(tree$prediction, nrow(newdata))

    if (return_probs) {
      # Vectorized probability matrix creation
      prob_matrix <- matrix(rep(tree$class_prob, nrow(newdata)),
        nrow = nrow(newdata), byrow = TRUE
      )
      colnames(prob_matrix) <- names(tree$class_prob)

      return(list(predictions = pred, probabilities = prob_matrix))
    }
    return(pred)
  }

  if (nrow(newdata) == 0) {
    if (return_probs) {
      all_classes <- get_all_classes(tree)
      return(list(
        predictions = character(0),
        probabilities = matrix(
          nrow = 0, ncol = length(all_classes),
          dimnames = list(NULL, all_classes)
        )
      ))
    }
    return(character(0))
  }

  # Scale features
  available_features <- intersect(tree$features, names(newdata))
  missing_features   <- setdiff(tree$features, names(newdata))

  if (length(missing_features) > 0) {
    warning("Prediction: missing features in newdata: ",
            paste(missing_features, collapse = ", "),
            " - using majority class fallback")
    # fall through to the scaling failure handler below
    X_scaled <- data.frame()
  } else {
    X_scaled <- apply_scaler(newdata[, tree$features, drop = FALSE], tree$scaler)
  }

  # Handle case where scaling fails
  if (ncol(X_scaled) == 0 || nrow(X_scaled) == 0) {
    warning("Scaling failed in prediction, using majority class")
    all_classes <- get_all_classes(tree)

    # FIXED: Better fallback - use tree's class distribution
    if (!is.null(tree$model) && !is.null(tree$model$fitted)) {
      fitted_table <- table(tree$model$fitted)
      majority_class <- names(which.max(fitted_table))
    } else {
      majority_class <- all_classes[1]
    }

    pred <- rep(majority_class, nrow(newdata))

    if (return_probs) {
      prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(all_classes))
      colnames(prob_matrix) <- all_classes
      prob_matrix[, majority_class] <- 1
      return(list(predictions = pred, probabilities = prob_matrix))
    }
    return(pred)
  }

  svm_result <- predict(tree$model, X_scaled,
    decision.values = TRUE,
    probability = TRUE
  )
  dec_values <- attr(svm_result, "decision.values")

  # For binary: use first column (or specified best_col)
  # For multiclass: use one-vs-rest approach
  if (is.matrix(dec_values)) {
    # Multiple decision values (one-vs-one for multiclass)
    best_col <- purrr::`%||%`(tree$best_col, 1)
    if (best_col > ncol(dec_values)) best_col <- 1
    decision_values <- dec_values[, best_col]
  } else {
    decision_values <- as.numeric(dec_values)
  }

  svm_probs <- attr(svm_result, "probabilities")

  # This MUST match the logic used during training in svm_split
  left_idx <- which(decision_values > 0)
  right_idx <- which(decision_values <= 0)

  pred <- vector("character", nrow(newdata))

  if (return_probs) {
    all_classes <- get_all_classes(tree)
    prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(all_classes))
    colnames(prob_matrix) <- all_classes

    # ################# LEFT BRANCH #################
    if (!is.null(tree$left) && length(left_idx) > 0) {
      # Recursive prediction
      left_result <- svm_predict_tree(
        tree$left,
        newdata[left_idx, , drop = FALSE],
        return_probs = TRUE,
        calibrate_probs
      )
      pred[left_idx] <- left_result$predictions
      prob_matrix[left_idx, ] <- left_result$probabilities
    } else if (length(left_idx) > 0) {
      left_svm_probs <- if (!is.null(svm_probs)) svm_probs[left_idx, , drop = FALSE] else NULL

      fallback_result <- get_fallback_predictions(
        model = tree$model,
        X_scaled = X_scaled[left_idx, , drop = FALSE],
        decision_values = decision_values[left_idx],
        svm_probs = left_svm_probs,
        all_classes = all_classes,
        calibrate = calibrate_probs
      )
      pred[left_idx] <- fallback_result$predictions
      prob_matrix[left_idx, ] <- fallback_result$probabilities
    }

    # ################# RIGHT BRANCH #################
    if (!is.null(tree$right) && length(right_idx) > 0) {
      # Recursive prediction
      right_result <- svm_predict_tree(
        tree$right,
        newdata[right_idx, , drop = FALSE],
        return_probs = TRUE,
        calibrate_probs
      )
      pred[right_idx] <- right_result$predictions
      prob_matrix[right_idx, ] <- right_result$probabilities
    } else if (length(right_idx) > 0) {
      right_svm_probs <- if (!is.null(svm_probs)) svm_probs[right_idx, , drop = FALSE] else NULL

      fallback_result <- get_fallback_predictions(
        model = tree$model,
        X_scaled = X_scaled[right_idx, , drop = FALSE],
        decision_values = decision_values[right_idx],
        svm_probs = right_svm_probs,
        all_classes = all_classes,
        calibrate = calibrate_probs
      )
      pred[right_idx] <- fallback_result$predictions
      prob_matrix[right_idx, ] <- fallback_result$probabilities
    }

    return(list(predictions = pred, probabilities = prob_matrix))
  } else {
    ################# PREDICTION ONLY (NO PROBABILITIES) #################

    # Left branch
    if (!is.null(tree$left) && length(left_idx) > 0) {
      pred[left_idx] <- svm_predict_tree(tree$left, newdata[left_idx, , drop = FALSE],
        return_probs = FALSE, calibrate_probs
      )
    } else if (length(left_idx) > 0) {
      # Use majority class from model
      pred[left_idx] <- rep(names(which.max(table(tree$model$fitted))), length(left_idx))
    }

    # Right branch
    if (!is.null(tree$right) && length(right_idx) > 0) {
      pred[right_idx] <- svm_predict_tree(tree$right, newdata[right_idx, , drop = FALSE],
        return_probs = FALSE, calibrate_probs
      )
    } else if (length(right_idx) > 0) {
      # Use majority class from model
      pred[right_idx] <- rep(names(which.max(table(tree$model$fitted))), length(right_idx))
    }

    return(pred)
  }
}

#' Fallback predictions for SVM decision tree nodes
#'
#' Generates class predictions and probabilities when SVM predictions are unavailable
#' or insufficient. This function is intended for internal use within the SVM tree.
#'
#' @param model An \code{svm} object fitted with training data.
#' @param X_scaled Scaled predictor matrix for the current node.
#' @param decision_values Numeric vector of SVM decision values.
#' @param svm_probs Optional SVM probability matrix (from \code{predict(..., probability=TRUE)}).
#' @param all_classes Character vector of all possible classes.
#' @param calibrate Logical; if \code{TRUE}, calibrates decision values into probabilities.
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{predictions}: Character vector of predicted classes.
#'   \item \code{probabilities}: Matrix of class probabilities (rows = samples, columns = classes).
#' }
#'
#' @keywords internal
get_fallback_predictions <- function(model, X_scaled, decision_values,
                                     svm_probs = NULL, all_classes,
                                     calibrate = TRUE) {
  n_samples <- length(decision_values)

  if (n_samples == 0) {
    return(list(
      predictions = character(0),
      probabilities = matrix(0,
        nrow = 0, ncol = length(all_classes),
        dimnames = list(NULL, all_classes)
      )
    ))
  }

  prob_matrix <- matrix(0, nrow = n_samples, ncol = length(all_classes))
  colnames(prob_matrix) <- all_classes

  # ################# OPTION 1: Use SVM's built-in probabilities #################
  if (!is.null(svm_probs) && is.matrix(svm_probs) && nrow(svm_probs) == n_samples) {
    svm_classes <- colnames(svm_probs)

    convert_decision_to_probs <- function(decision_values, model = NULL) {
      if (length(decision_values) == 0) {
        return(numeric(0))
      }

      # If we have the model, use training statistics for calibration
      if (!is.null(model) && !is.null(model$decision.values)) {
        train_dec <- as.numeric(model$decision.values)

        train_dec <- train_dec[!is.na(train_dec)]

        if (length(train_dec) > 0) {
          dec_mean <- mean(train_dec)
          dec_sd <- sd(train_dec)

          if (is.na(dec_sd) || dec_sd == 0) {
            dec_sd <- 1
          }

          # Standardize decision values
          scaled_dec <- (decision_values - dec_mean) / dec_sd
          probs <- 1 / (1 + exp(-scaled_dec))
        } else {
          # Fallback if training data unavailable
          probs <- 1 / (1 + exp(-decision_values))
        }
      } else {
        # Simple sigmoid without calibration
        probs <- 1 / (1 + exp(-decision_values))
      }

      probs <- pmax(pmin(probs, 0.999), 0.001)

      return(probs)
    }
    for (cls in intersect(svm_classes, all_classes)) {
      prob_matrix[, cls] <- svm_probs[, cls]
    }

    row_sums <- rowSums(prob_matrix)
    if (any(row_sums > 0)) {
      prob_matrix[row_sums > 0, ] <- prob_matrix[row_sums > 0, ] / row_sums[row_sums > 0]
    }

    # Get predictions from probabilities
    pred_classes <- all_classes[apply(prob_matrix, 1, which.max)]

    return(list(predictions = pred_classes, probabilities = prob_matrix))
  }

  # ################# OPTION 2: Calibrated decision values #################
  if (calibrate && !is.null(model)) {
    probs <- convert_decision_to_probs(decision_values, model)

    model_classes <- levels(model$fitted)

    if (length(model_classes) == 2) {
      # Binary classification
      # Positive decision -> high probability for first class
      prob_matrix[, model_classes[1]] <- probs
      prob_matrix[, model_classes[2]] <- 1 - probs
    } else {
      # Use one-vs-rest approach with softmax
      # For now, distribute based on calibrated probability
      for (cls in model_classes) {
        if (cls %in% all_classes) {
          prob_matrix[, cls] <- probs / length(model_classes)
        }
      }

      # Normalize
      row_sums <- rowSums(prob_matrix)
      if (any(row_sums > 0)) {
        prob_matrix[row_sums > 0, ] <- prob_matrix[row_sums > 0, ] / row_sums[row_sums > 0]
      }
    }

    pred_classes <- all_classes[apply(prob_matrix, 1, which.max)]

    return(list(predictions = pred_classes, probabilities = prob_matrix))
  }

  # ################# OPTION 3: Last resort - use training class proportions #################
  if (!is.null(model) && !is.null(model$fitted)) {
    fitted_table <- table(model$fitted)
    best_class <- names(which.max(fitted_table))
    class_probs <- as.numeric(prop.table(fitted_table))
    names(class_probs) <- names(fitted_table)

    # Assign same probabilities to all samples (not ideal but consistent)
    for (cls in names(class_probs)) {
      if (cls %in% all_classes) {
        prob_matrix[, cls] <- class_probs[cls]
      }
    }

    pred_classes <- rep(best_class, n_samples)
  } else {
    # Absolute fallback - uniform distribution
    prob_matrix[] <- 1 / length(all_classes)
    pred_classes <- rep(all_classes[1], n_samples)
  }

  return(list(predictions = pred_classes, probabilities = prob_matrix))
}

#' Convert SVM decision values to probabilities
#'
#' Converts numeric SVM decision values into probabilities using a logistic/sigmoid
#' transformation. Optionally uses the model's training decision values for calibration.
#' Intended for internal use within the SVM tree prediction workflow.
#'
#' @param decision_values Numeric vector of decision values.
#' @param model Optional \code{svm} object; if provided, training decision values
#'   are used to calibrate scaling.
#'
#' @return Numeric vector of probabilities, clipped between 0.001 and 0.999.
#'
#' @keywords internal
convert_decision_to_probs <- function(decision_values, model = NULL) {
  if (length(decision_values) == 0) {
    return(numeric(0))
  }

  # If we have the model, use training statistics for calibration
  if (!is.null(model) && !is.null(model$decision.values)) {
    train_dec <- as.numeric(model$decision.values)


    train_dec <- train_dec[!is.na(train_dec)]

    if (length(train_dec) > 0) {
      dec_mean <- mean(train_dec)
      dec_sd <- sd(train_dec)


      if (is.na(dec_sd) || dec_sd == 0) {
        dec_sd <- 1
      }

      # Standardize decision values
      scaled_dec <- (decision_values - dec_mean) / dec_sd
      probs <- 1 / (1 + exp(-scaled_dec))
    } else {
      # Fallback if training data unavailable
      probs <- 1 / (1 + exp(-decision_values))
    }
  } else {
    # Simple sigmoid without calibration
    probs <- 1 / (1 + exp(-decision_values))
  }

  probs <- pmax(pmin(probs, 0.999), 0.001)

  return(probs)
}
