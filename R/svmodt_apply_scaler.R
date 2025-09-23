apply_scaler <- function(df, scaler) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  as.data.frame(scaler$transform(df))
}
