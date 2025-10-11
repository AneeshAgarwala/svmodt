scale_node <- function(df) {
  if (ncol(df) == 0 || nrow(df) == 0) {
    return(list(
      train = data.frame(),
      transform = function(newdata) data.frame()
    ))
  }

  # Identify and remove constant features
  constant_vars <- sapply(df, function(x) {
    if (is.numeric(x)) {
      var(x, na.rm = TRUE) == 0 || all(is.na(x))
    } else {
      length(unique(x[!is.na(x)])) <= 1
    }
  })

  if (any(constant_vars)) {
    warning(paste("Removing constant features:", paste(names(df)[constant_vars], collapse = ", ")))
    df <- df[, !constant_vars, drop = FALSE]
  }

  if (ncol(df) == 0) {
    return(list(
      train = data.frame(),
      transform = function(newdata) data.frame()
    ))
  }

  mu <- sapply(df, mean, na.rm = TRUE)
  sd_vals <- sapply(df, sd, na.rm = TRUE)

  # FIXED: Handle zero standard deviation
  sd_vals[sd_vals == 0] <- 1

  scaled <- sweep(sweep(df, 2, mu, "-"), 2, sd_vals, "/")

  list(
    train = scaled,
    transform = function(newdata) {
      if (ncol(newdata) == 0 || nrow(newdata) == 0) return(data.frame())

      common_cols <- intersect(names(newdata), names(mu))
      if (length(common_cols) == 0) return(data.frame())

      newdata_subset <- newdata[, common_cols, drop = FALSE]
      sweep(sweep(newdata_subset, 2, mu[common_cols], "-"), 2, sd_vals[common_cols], "/")
    }
  )
}
