#' @title Fit a linear SVM model with optional class weights
#' @description
#' Fits a linear Support Vector Machine (SVM) classifier using the \pkg{e1071} package,
#' with optional class-specific weights to handle class imbalance.
#'
#' @param X A data frame or matrix of predictor variables.
#' @param y A vector of class labels corresponding to the rows of \code{X}.
#' @param class_weights Optional named numeric vector of class weights. Names must match
#'   the unique class labels in \code{y}. Weights are capped at 10 to prevent instability.
#' @param verbose Logical; if \code{TRUE}, prints diagnostic messages during fitting.
#' @param ... Additional arguments passed to \code{\link[e1071]{svm}}.
#'
#' @return A fitted \code{svm} model object (of class \code{"svm"}) on success, or
#'   \code{NULL} if fitting fails.
#'
#' @details
#' - Uses a **linear kernel** by default.
#' - Enables decision values and probability estimates.
#' - Scaling is disabled (\code{scale = FALSE}).
#' - When \code{class_weights} is supplied, weights are capped at 10 and passed to
#'   \code{\link[e1071]{svm}} via its \code{class.weights} parameter.
#' - Returns \code{NULL} if data is empty or model fitting fails.
#'
#' @examples
#' \dontrun{
#' library(e1071)
#' set.seed(1)
#' X <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
#' y <- factor(sample(c("A", "B"), 100, replace = TRUE))
#' weights <- c(A = 1, B = 3)
#' model <- fit_svm_with_weights(X, y, class_weights = weights, verbose = TRUE)
#' }
#'
#' @keywords internal
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
    e1071::svm(x = X, y = y,
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
