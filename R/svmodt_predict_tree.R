svm_predict_tree <- function(tree, newdata, return_probs = FALSE,
                                      calibrate_probs = TRUE) {

  # Handle leaf nodes
  if (tree$is_leaf) {
    pred <- rep(tree$prediction, nrow(newdata))

    if (return_probs) {
      prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(tree$class_prob))
      colnames(prob_matrix) <- names(tree$class_prob)

      # Use stored probabilities (these are still class proportions)
      for (i in 1:nrow(newdata)) {
        prob_matrix[i, ] <- tree$class_prob
      }
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

  # Get SVM predictions and decision values
  svm_pred <- predict(tree$model, X_scaled, decision.values = TRUE, probability = TRUE)
  dec <- attr(svm_pred, "decision.values")
  dec_vals <- if (is.matrix(dec)) dec[, tree$best_col] else as.numeric(dec)

  # IMPROVEMENT: Get SVM probability estimates if available
  svm_probs <- attr(svm_pred, "probabilities")

  left_idx  <- which(dec_vals > 0)
  right_idx <- which(dec_vals <= 0)

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
      # IMPROVEMENT: Use SVM probabilities or calibrated decision values
      fallback_result <- get_fallback(tree$model, X_scaled[left_idx, , drop = FALSE],
                                               dec_vals[left_idx], svm_probs, left_idx,
                                               all_classes, calibrate_probs)
      pred[left_idx] <- fallback_result$predictions
      for (i in seq_along(left_idx)) {
        prob_matrix[left_idx[i], ] <- fallback_result$probabilities[i, ]
      }
    }

    # Predict recursively for right branch
    if (!is.null(tree$right) && length(right_idx) > 0) {
      right_result <- svm_predict_tree(tree$right, newdata[right_idx, , drop = FALSE],
                                                return_probs = TRUE, calibrate_probs)
      pred[right_idx] <- right_result$predictions
      prob_matrix[right_idx, ] <- right_result$probabilities

    } else if (length(right_idx) > 0) {
      # IMPROVEMENT: Use SVM probabilities or calibrated decision values
      fallback_result <- get_fallback(tree$model, X_scaled[right_idx, , drop = FALSE],
                                               dec_vals[right_idx], svm_probs, right_idx,
                                               all_classes, calibrate_probs)
      pred[right_idx] <- fallback_result$predictions
      for (i in seq_along(right_idx)) {
        prob_matrix[right_idx[i], ] <- fallback_result$probabilities[i, ]
      }
    }

    return(list(predictions = pred, probabilities = prob_matrix))

  } else {
    # Original prediction-only logic
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

# Helper function to get fallback prediction and probabilities
get_fallback <- function(model, X_scaled, dec_vals, svm_probs, indices,
                                  all_classes, calibrate = TRUE) {

  # Try to use SVM's built-in probability estimates
  if (!is.null(svm_probs) && is.matrix(svm_probs)) {
    # SVM provides probabilities - use them directly
    n_samples <- length(indices)
    prob_matrix <- matrix(0, nrow = n_samples, ncol = length(all_classes))
    colnames(prob_matrix) <- all_classes

    # Map SVM probabilities to our class order
    svm_classes <- colnames(svm_probs)
    for (cls in intersect(svm_classes, all_classes)) {
      prob_matrix[, cls] <- svm_probs[indices, cls]
    }

    # Get predictions from probabilities
    pred_classes <- all_classes[apply(prob_matrix, 1, which.max)]

    return(list(predictions = pred_classes, probabilities = prob_matrix))
  }

  # Fallback: Convert decision values to probabilities
  if (calibrate) {
    # Use sigmoid/Platt scaling to convert decision values to probabilities
    probs <- decision_values_to_probs(dec_vals, model)

    # Create probability matrix
    n_samples <- length(dec_vals)
    prob_matrix <- matrix(0, nrow = n_samples, ncol = length(all_classes))
    colnames(prob_matrix) <- all_classes

    # Assign probabilities based on model classes
    model_classes <- levels(model$fitted)
    if (length(model_classes) == 2) {
      # Binary classification
      prob_matrix[, model_classes[1]] <- probs
      prob_matrix[, model_classes[2]] <- 1 - probs
    } else {
      # Multi-class: use softmax on decision values
      warning("Multi-class probability estimation is approximate")
      prob_matrix[, model_classes[1]] <- probs
      prob_matrix[, model_classes[2]] <- 1 - probs
    }

    pred_classes <- all_classes[apply(prob_matrix, 1, which.max)]

    return(list(predictions = pred_classes, probabilities = prob_matrix))
  }

  # Last resort: Use class proportions from fitted values
  fitted_table <- table(model$fitted)
  best_class <- names(which.max(fitted_table))
  probs <- as.numeric(prop.table(fitted_table))
  names(probs) <- names(fitted_table)

  n_samples <- length(dec_vals)
  prob_matrix <- matrix(0, nrow = n_samples, ncol = length(all_classes))
  colnames(prob_matrix) <- all_classes

  # Same probability for all samples (not ideal)
  for (i in 1:n_samples) {
    prob_matrix[i, names(probs)] <- probs
  }

  pred_classes <- rep(best_class, n_samples)

  return(list(predictions = pred_classes, probabilities = prob_matrix))
}

# Convert SVM decision values to probabilities using sigmoid
decision_values_to_probs <- function(decision_values, model = NULL) {

  # Method 1: Simple sigmoid
  # Maps decision values to [0, 1] range
  # Positive decision -> high probability for positive class
  # Negative decision -> low probability for positive class

  # Standard sigmoid: 1 / (1 + exp(-x))
  # We can scale it for better calibration

  # If we have the model, estimate scaling parameters from training data
  if (!is.null(model) && !is.null(model$decision.values)) {
    # Get decision values from training data
    train_dec <- model$decision.values
    train_labels <- as.numeric(model$fitted == levels(model$fitted)[1])

    # Estimate Platt scaling parameters (simplified)
    # A and B such that P(y=1|f) = 1 / (1 + exp(A*f + B))
    # For simplicity, we'll just use standardized sigmoid

    # Standardize decision values
    dec_mean <- mean(train_dec, na.rm = TRUE)
    dec_sd <- sd(train_dec, na.rm = TRUE)
    if (dec_sd == 0) dec_sd <- 1

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
