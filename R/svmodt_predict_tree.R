svm_predict_tree <- function(tree, newdata) {
  if (tree$is_leaf) {
    return(rep(tree$prediction, nrow(newdata)))
  }

  if (nrow(newdata) == 0) return(character(0))

  # Scale features

  X_scaled <- apply_scaler(newdata[, tree$features, drop = FALSE], tree$scaler)

  # SVM decision values
  dec <- attr(predict(tree$model, X_scaled, decision.values = TRUE), "decision.values")
  dec_vals <- if (is.matrix(dec)) dec[, tree$best_col] else as.numeric(dec)

  left_idx  <- which(dec_vals > 0)
  right_idx <- which(dec_vals <= 0)

  pred <- vector("character", nrow(newdata))

  # Predict recursively
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

  pred
}
