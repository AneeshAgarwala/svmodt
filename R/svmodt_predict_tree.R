svm_predict_tree <- function(tree, newdata, return_probs = FALSE) {
  if (tree$is_leaf) {
    pred <- rep(tree$prediction, nrow(newdata))
    if (return_probs) {
      # Create probability matrix from leaf node
      prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(tree$class_prob))
      colnames(prob_matrix) <- names(tree$class_prob)
      # Assign the same probabilities to each row
      for (i in 1:nrow(newdata)) {
        prob_matrix[i, ] <- tree$class_prob
      }
      return(list(predictions = pred, probabilities = prob_matrix))
    }
    return(pred)
  }

  if (nrow(newdata) == 0) {
    if (return_probs) {
      return(list(predictions = character(0),
                  probabilities = matrix(nrow = 0, ncol = length(names(tree$class_prob)))))
    }
    return(character(0))
  }

  # Scale features
  X_scaled <- apply_scaler(newdata[, tree$features, drop = FALSE], tree$scaler)

  # SVM decision values
  dec <- attr(predict(tree$model, X_scaled, decision.values = TRUE), "decision.values")
  dec_vals <- if (is.matrix(dec)) dec[, tree$best_col] else as.numeric(dec)

  left_idx  <- which(dec_vals > 0)
  right_idx <- which(dec_vals <= 0)

  pred <- vector("character", nrow(newdata))

  if (return_probs) {
    # Get all possible class names from the tree structure
    all_classes <- get_all_classes(tree)
    prob_matrix <- matrix(0, nrow = nrow(newdata), ncol = length(all_classes))
    colnames(prob_matrix) <- all_classes

    # Predict recursively for left branch
    if (!is.null(tree$left) && length(left_idx) > 0) {
      left_result <- svm_predict_tree(tree$left, newdata[left_idx, , drop = FALSE],
                                      return_probs = TRUE)
      pred[left_idx] <- left_result$predictions
      prob_matrix[left_idx, ] <- left_result$probabilities
    } else if (length(left_idx) > 0) {
      # Fallback: use current node's model fitted values
      fallback_pred <- get_fallback_prediction(tree$model)
      pred[left_idx] <- rep(fallback_pred$class, length(left_idx))
      # Correctly assign probabilities for each row
      for (i in left_idx) {
        prob_matrix[i, names(fallback_pred$probs)] <- fallback_pred$probs
      }
    }

    # Predict recursively for right branch
    if (!is.null(tree$right) && length(right_idx) > 0) {
      right_result <- svm_predict_tree(tree$right, newdata[right_idx, , drop = FALSE],
                                       return_probs = TRUE)
      pred[right_idx] <- right_result$predictions
      prob_matrix[right_idx, ] <- right_result$probabilities
    } else if (length(right_idx) > 0) {
      # Fallback: use current node's model fitted values
      fallback_pred <- get_fallback_prediction(tree$model)
      pred[right_idx] <- rep(fallback_pred$class, length(right_idx))
      # Correctly assign probabilities for each row
      for (i in right_idx) {
        prob_matrix[i, names(fallback_pred$probs)] <- fallback_pred$probs
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

# Helper function to extract all possible class names from the tree
get_all_classes <- function(tree) {
  if (tree$is_leaf) {
    return(names(tree$class_prob))
  }

  classes <- character(0)
  if (!is.null(tree$left)) {
    classes <- c(classes, get_all_classes(tree$left))
  }
  if (!is.null(tree$right)) {
    classes <- c(classes, get_all_classes(tree$right))
  }

  return(unique(classes))
}

# Helper function to get fallback prediction and probabilities
get_fallback_prediction <- function(model) {
  fitted_table <- table(model$fitted)
  best_class <- names(which.max(fitted_table))

  # Create probability vector
  all_classes <- names(fitted_table)
  probs <- as.numeric(prop.table(fitted_table))
  names(probs) <- all_classes

  return(list(class = best_class, probs = probs))
}

# Convenience wrapper functions
predict_class <- function(tree, newdata) {
  svm_predict_tree(tree, newdata, return_probs = FALSE)
}

predict_proba <- function(tree, newdata) {
  result <- svm_predict_tree(tree, newdata, return_probs = TRUE)
  return(result$probabilities)
}

predict_both <- function(tree, newdata) {
  svm_predict_tree(tree, newdata, return_probs = TRUE)
}
