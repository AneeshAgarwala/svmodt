svm_predict_tree <- function(tree, newdata, return_probs = FALSE,
                             calibrate_probs = TRUE) {

  # Handle leaf nodes
  if (tree$is_leaf) {
    pred <- rep(tree$prediction, nrow(newdata))

    if (return_probs) {
      # Vectorized probability matrix creation
      prob_matrix <- matrix(rep(tree$class_prob, nrow(newdata)),
                            nrow = nrow(newdata), byrow = TRUE)
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
        probabilities = matrix(nrow = 0, ncol = length(all_classes),
                               dimnames = list(NULL, all_classes))
      ))
    }
    return(character(0))
  }

  # Scale features
  X_scaled <- apply_scaler(newdata[, tree$features, drop = FALSE], tree$scaler)

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

  svm_result <- predict(tree$model, X_scaled, decision.values = TRUE,
                        probability = TRUE)
  dec_values <- attr(svm_result, "decision.values")

  # For binary: use first column (or specified best_col)
  # For multiclass: use one-vs-rest approach
  if (is.matrix(dec_values)) {
    # Multiple decision values (one-vs-one for multiclass)
    best_col <- tree$best_col %||% 1
    if (best_col > ncol(dec_values)) best_col <- 1
    decision_values <- dec_values[, best_col]
  } else {
    decision_values <- as.numeric(dec_values)
  }

  svm_probs <- attr(svm_result, "probabilities")

  # This MUST match the logic used during training in svm_split
  left_idx  <- which(decision_values > 0)
  right_idx <- which(decision_values <= 0)

  pred <- vector("character", nrow(newdata))

  if (return_probs) {
    all_classes <- get_all_classes(tree)
    prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(all_classes))
    colnames(prob_matrix) <- all_classes

    # ========== LEFT BRANCH ==========
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

    # ========== RIGHT BRANCH ==========
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
      # FIXED: Pass correct subset of probabilities
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
    # ========== PREDICTION ONLY (NO PROBABILITIES) ==========

    # Left branch
    if (!is.null(tree$left) && length(left_idx) > 0) {
      pred[left_idx] <- svm_predict_tree(tree$left, newdata[left_idx, , drop = FALSE],
                                         return_probs = FALSE, calibrate_probs)
    } else if (length(left_idx) > 0) {
      # Use majority class from model
      pred[left_idx] <- rep(names(which.max(table(tree$model$fitted))), length(left_idx))
    }

    # Right branch
    if (!is.null(tree$right) && length(right_idx) > 0) {
      pred[right_idx] <- svm_predict_tree(tree$right, newdata[right_idx, , drop = FALSE],
                                          return_probs = FALSE, calibrate_probs)
    } else if (length(right_idx) > 0) {
      # Use majority class from model
      pred[right_idx] <- rep(names(which.max(table(tree$model$fitted))), length(right_idx))
    }

    return(pred)
  }
}


get_fallback_predictions <- function(model, X_scaled, decision_values,
                                     svm_probs = NULL, all_classes,
                                     calibrate = TRUE) {

  n_samples <- length(decision_values)

  if (n_samples == 0) {
    return(list(
      predictions = character(0),
      probabilities = matrix(0, nrow = 0, ncol = length(all_classes),
                             dimnames = list(NULL, all_classes))
    ))
  }

  prob_matrix <- matrix(0, nrow = n_samples, ncol = length(all_classes))
  colnames(prob_matrix) <- all_classes

  # ========== OPTION 1: Use SVM's built-in probabilities ==========
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

  # ========== OPTION 2: Calibrated decision values ==========
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

  # ========== OPTION 3: Last resort - use training class proportions ==========
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
