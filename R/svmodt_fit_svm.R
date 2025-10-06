library(e1071)

fit_svm_with_weights <- function(X, y, class_weights, verbose, ...) {

  # If no weights provided, calculate balanced weights as default
  if (is.null(class_weights)) {
    class_counts <- table(y)
    n_samples <- length(y)
    n_classes <- length(class_counts)

    # Default: balanced weights
    class_weights <- n_samples / (n_classes * as.numeric(class_counts))
    names(class_weights) <- names(class_counts)
  }

  # Cap extreme weights to prevent numerical issues
  class_weights <- pmin(class_weights, 10)

  if (verbose) {
    cat("SVM fitting with weights:", paste(names(class_weights), "=", round(class_weights, 3), collapse = ", "), "\n")
  }

  tryCatch(
    svm(x = X, y = y,
        kernel = "linear",
        decision.values = TRUE,
        probability = TRUE,
        scale = FALSE,
        class.weights = class_weights,
        ...),
    error = function(e) {
      if (verbose) cat("SVM failed:", e$message, "\n")
      NULL
    }
  )
}
