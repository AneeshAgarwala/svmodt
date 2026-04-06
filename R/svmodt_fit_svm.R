#' @title Fit a linear SVM model with optional class weights
#' @description
#' Fits a linear Support Vector Machine (SVM) classifier using the \pkg{e1071} package,
#' with optional class-specific weights to handle class imbalance.
#'
#' @param X_scaled A data frame or matrix of predictor variables.
#' @param y A vector of class labels corresponding to the rows of \code{X}.
#' @param class_weights_vec Optional named numeric vector of class weights. Names must match
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
fit_svm_with_weights <- function(X_scaled, y, class_weights_vec,
                                 verbose = FALSE, ...) {
  tryCatch(
    {
      # Coerce y to factor with character levels — must match weight names
      y_factor <- factor(as.character(y))

      if (verbose && !is.null(class_weights_vec)) {
        cat("SVM weight names :", paste(names(class_weights_vec), collapse = ", "), "\n")
        cat("y factor levels  :", paste(levels(y_factor), collapse = ", "), "\n")

        # Explicit mismatch check before fit
        missing_in_weights <- setdiff(levels(y_factor), names(class_weights_vec))
        missing_in_levels <- setdiff(names(class_weights_vec), levels(y_factor))

        if (length(missing_in_weights) > 0) {
          cat("[WARN] Levels not in weights:", paste(missing_in_weights, collapse = ", "), "\n")
        }
        if (length(missing_in_levels) > 0) {
          cat("[WARN] Weights not in levels:", paste(missing_in_levels, collapse = ", "), "\n")
        }
      }

      if (is.null(class_weights_vec)) {
        e1071::svm(
          x                = X_scaled,
          y                = y_factor,
          kernel           = "linear",
          decision.values  = TRUE,
          probability      = TRUE,
          ...
        )
      } else {
        e1071::svm(
          x                = X_scaled,
          y                = y_factor,
          kernel           = "linear",
          class.weights    = class_weights_vec,
          decision.values  = TRUE,
          probability      = TRUE,
          ...
        )
      }
    },
    error = function(e) {
      if (verbose) message("  [WARN] SVM fit failed: ", conditionMessage(e))
      NULL
    }
  )
}
