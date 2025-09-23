library(e1071)

fit_svm <- function(X, y, verbose, ...) {
  class_counts  <- table(y)
  w <- 1 / as.numeric(class_counts)
  names(w) <- names(class_counts)
  w <- w / sum(w)
  if (verbose) cat("Class weights:",
                   paste(names(w), round(w, 3), collapse = ", "), "\n")
  tryCatch(
    svm(x = X, y = y,
        kernel = "linear",
        decision.values = TRUE,
        probability = TRUE,
        scale = FALSE,
        class.weights = w,
        ...),
    error = function(e) { if (verbose) cat("SVM failed:", e$message, "\n"); NULL }
  )
}
