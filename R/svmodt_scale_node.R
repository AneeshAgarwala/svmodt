scale_node <- function(df) {
  mu  <- sapply(df, mean, na.rm = TRUE)
  sdv <- sapply(df, sd,   na.rm = TRUE)
  sdv[sdv == 0] <- 1

  transform <- function(new) {
    # keep same columns and order
    new <- new[, names(mu), drop = FALSE]
    sweep(sweep(new, 2, mu, "-"), 2, sdv, "/")
  }

  list(
    train     = transform(df),
    center    = mu,
    scale     = sdv,
    transform = transform
  )
}
