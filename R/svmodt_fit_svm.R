library(e1071)

fit_svm_with_weights <- function(X, y, class_weights, verbose, ...) {
  # If no weights provided, use NULL (equal weights)
  if (is.null(class_weights)) {
    if (verbose) {
      cat("Class weights: none (equal weights)\n")
    }

    tryCatch(
      svm(x = X, y = y,
          kernel = "linear",
          decision.values = TRUE,
          probability = TRUE,
          scale = FALSE,
          # No class.weights parameter - equal weights
          ...),
      error = function(e) {
        if (verbose) cat("SVM failed:", e$message, "\n")
        NULL
      }
    )
  } else {
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
}
