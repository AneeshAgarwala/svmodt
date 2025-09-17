scale_node <- function(df) {
  mu <- sapply(df, mean, na.rm = TRUE)
  sdv <- sapply(df, sd, na.rm = TRUE)
  sdv[sdv == 0] <- 1
  scaled <- sweep(sweep(df, 2, mu), 2, sdv, "/")
  list(train = scaled, center = mu, scale = sdv)
}
