library(e1071)

fit_svm_with_weights <- function(X, y, class_weights = NULL, verbose = FALSE, ...) {
  if (nrow(X) == 0 || length(y) == 0) {
    if (verbose) cat("Cannot fit SVM: empty data\n")
    return(NULL)
  }

  # FIXED: Cap weights consistently
  if (!is.null(class_weights)) {
    class_weights <- pmin(class_weights, 10)
    if (verbose) {
      cat("SVM fitting with weights:",
          paste(names(class_weights), "=", round(class_weights, 3), collapse = ", "), "\n")
    }
  } else {
    if (verbose) cat("SVM fitting with equal weights\n")
  }

  tryCatch({
    svm(x = X, y = y,
        kernel = "linear",
        decision.values = TRUE,
        probability = TRUE,
        scale = FALSE,
        class.weights = class_weights,
        ...)
  }, error = function(e) {
    if (verbose) cat("SVM failed:", e$message, "\n")
    NULL
  })
}
