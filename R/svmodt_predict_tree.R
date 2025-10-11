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
      return(list(predictions = character(0),
                  probabilities = matrix(nrow = 0, ncol = length(all_classes))))
    }
    return(character(0))
  }

  # Scale features
  X_scaled <- apply_scaler(newdata[, tree$features, drop = FALSE], tree$scaler)

  # Handle case where scaling fails
  if (ncol(X_scaled) == 0 || nrow(X_scaled) == 0) {
    warning("Scaling failed in prediction, using majority class")
    all_classes <- get_all_classes(tree)
    majority_class <- all_classes[1]
    pred <- rep(majority_class, nrow(newdata))

    if (return_probs) {
      prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(all_classes))
      colnames(prob_matrix) <- all_classes
      prob_matrix[, majority_class] <- 1
      return(list(predictions = pred, probabilities = prob_matrix))
    }
    return(pred)
  }

  # Get SVM predictions and decision values
  svm_pred <- predict(tree$model, X_scaled, decision.values = TRUE, probability = TRUE)
  dec <- attr(svm_pred, "decision.values")
  decision_values <- if (is.matrix(dec)) dec[, tree$best_col] else as.numeric(dec)

  # Get SVM probability estimates if available
  svm_probs <- attr(svm_pred, "probabilities")

  left_idx  <- which(decision_values > 0)
  right_idx <- which(decision_values <= 0)

  pred <- vector("character", nrow(newdata))

  if (return_probs) {
    all_classes <- get_all_classes(tree)
    prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(all_classes))
    colnames(prob_matrix) <- all_classes

    # Predict recursively for left branch
    if (!is.null(tree$left) && length(left_idx) > 0) {
      left_result <- svm_predict_tree(tree$left, newdata[left_idx, , drop = FALSE],
                                      return_probs = TRUE, calibrate_probs)
      pred[left_idx] <- left_result$predictions
      prob_matrix[left_idx, ] <- left_result$probabilities

    } else if (length(left_idx) > 0) {
      fallback_result <- get_fallback_predictions(
        tree$model, X_scaled[left_idx, , drop = FALSE],
        decision_values[left_idx], svm_probs, left_idx,
        all_classes, calibrate_probs
      )
      pred[left_idx] <- fallback_result$predictions
      prob_matrix[left_idx, ] <- fallback_result$probabilities
    }

    # Predict recursively for right branch
    if (!is.null(tree$right) && length(right_idx) > 0) {
      right_result <- svm_predict_tree(tree$right, newdata[right_idx, , drop = FALSE],
                                       return_probs = TRUE, calibrate_probs)
      pred[right_idx] <- right_result$predictions
      prob_matrix[right_idx, ] <- right_result$probabilities

    } else if (length(right_idx) > 0) {
      fallback_result <- get_fallback_predictions(
        tree$model, X_scaled[right_idx, , drop = FALSE],
        decision_values[right_idx], svm_probs, right_idx,
        all_classes, calibrate_probs
      )
      pred[right_idx] <- fallback_result$predictions
      prob_matrix[right_idx, ] <- fallback_result$probabilities
    }

    return(list(predictions = pred, probabilities = prob_matrix))

  } else {
    # Prediction-only logic
    if (!is.null(tree$left) && length(left_idx) > 0) {
      pred[left_idx] <- svm_predict_tree(tree$left, newdata[left_idx, , drop = FALSE])
    } else if (length(left_idx) > 0) {
      pred[left_idx] <- rep(names(which.max(table(tree$model$fitted))), length(left_idx))
    }

    if (!is.null(tree$right) && length(right_idx) > 0) {
      pred[right_idx] <- svm_predict_tree(tree$right, newdata[right_idx, , drop = FALSE])
    } else if (length(right_idx) > 0) {
      pred[right_idx] <- rep(names(which.max(table(tree$model$fitted))), length(right_idx))
    }

    return(pred)
  }
}


get_fallback_predictions <- function(model, X_scaled, decision_values, svm_probs,
                                     indices, all_classes, calibrate = TRUE) {

  n_samples <- length(decision_values)
  prob_matrix <- matrix(0, nrow = n_samples, ncol = length(all_classes))
  colnames(prob_matrix) <- all_classes

  # Try to use SVM's built-in probability estimates
  if (!is.null(svm_probs) && is.matrix(svm_probs)) {
    svm_classes <- colnames(svm_probs)

    # Map SVM probabilities to our class order
    for (cls in intersect(svm_classes, all_classes)) {
      if (length(indices) <= nrow(svm_probs)) {
        prob_matrix[, cls] <- svm_probs[indices, cls]
      }
    }

    # Get predictions from probabilities
    pred_classes <- all_classes[apply(prob_matrix, 1, which.max)]

    return(list(predictions = pred_classes, probabilities = prob_matrix))
  }

  # Fallback: Convert decision values to probabilities
  if (calibrate) {
    probs <- convert_decision_to_probs(decision_values, model)

    model_classes <- levels(model$fitted)
    if (length(model_classes) == 2) {
      # Binary classification
      prob_matrix[, model_classes[1]] <- probs
      prob_matrix[, model_classes[2]] <- 1 - probs
    } else {

      # Use softmax on decision values
      for (i in seq_along(model_classes)) {
        if (model_classes[i] %in% all_classes) {
          prob_matrix[, model_classes[i]] <- probs / length(model_classes)
        }
      }
    }

    pred_classes <- all_classes[apply(prob_matrix, 1, which.max)]

    return(list(predictions = pred_classes, probabilities = prob_matrix))
  }

  # Last resort: Use class proportions from fitted values
  fitted_table <- table(model$fitted)
  best_class <- names(which.max(fitted_table))
  class_probs <- as.numeric(prop.table(fitted_table))
  names(class_probs) <- names(fitted_table)

  for (cls in names(class_probs)) {
    if (cls %in% all_classes) {
      prob_matrix[, cls] <- class_probs[cls]
    }
  }

  pred_classes <- rep(best_class, n_samples)

  return(list(predictions = pred_classes, probabilities = prob_matrix))
}

convert_decision_to_probs <- function(decision_values, model = NULL) {

  # If we have the model, estimate scaling parameters from training data
  if (!is.null(model) && !is.null(model$decision.values)) {
    train_dec <- model$decision.values
    train_labels <- as.numeric(model$fitted == levels(model$fitted)[1])

    # Standardize decision values
    dec_mean <- mean(train_dec, na.rm = TRUE)
    dec_sd <- sd(train_dec, na.rm = TRUE)
    if (dec_sd == 0 || is.na(dec_sd)) dec_sd <- 1

    scaled_dec <- (decision_values - dec_mean) / dec_sd
    probs <- 1 / (1 + exp(-scaled_dec))

  } else {
    # Simple sigmoid without calibration
    probs <- 1 / (1 + exp(-decision_values))
  }

  # Ensure probabilities are in valid range
  probs <- pmax(pmin(probs, 0.999), 0.001)

  return(probs)
}
