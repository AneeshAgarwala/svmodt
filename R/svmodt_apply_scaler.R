#' Apply a scaler transformation to a data frame
#'
#' This internal helper function applies a scaling transformation to a data frame
#' using a provided scaler object. It returns the unscaled data in case of failure.
#'
#' @param df A data frame containing numeric features to be scaled.
#' @param scaler A scaler object with a `transform` method or function used to scale the data.
#'
#' @return A scaled data frame. If scaling fails or invalid inputs are provided,
#'   the original (unscaled) data frame is returned.
#'
#' @details
#' This function is intended for internal use within the package and is not exported.
#' It wraps the scaler's `transform()` call in error handling to prevent failures
#' from interrupting higher-level processes.
#' @keywords internal
apply_scaler <- function(df, scaler) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }
  if (is.null(scaler) || is.null(scaler$transform)) {
    warning("Invalid scaler provided, returning unscaled data")
    return(df)
  }
  tryCatch(
    as.data.frame(scaler$transform(df)),
    error = function(e) {
      warning("Scaling failed: ", e$message)
      df
    }
  )
}
