apply_scaler <- function(df, scaler) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())

  # Use the transform function provided in scaler
  scaled <- scaler$transform(df)

  # Ensure result is a data.frame
  as.data.frame(scaled)
}
