apply_scaler <- function(df, scaler) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
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
