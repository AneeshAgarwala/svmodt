apply_scaler <- function(df, scaler) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  if (is.null(scaler) || is.null(scaler$transform) || !is.function(scaler$transform))
    return(df)  # nothing to scale
  as.data.frame(scaler$transform(df))
}
