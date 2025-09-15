scale_node <- function(train, test) {
  m <- colMeans(train)
  s <- apply(train, 2, sd)
  s[s == 0] <- 1
  list(
    train = sweep(sweep(train, 2, m, "-"), 2, s, "/"),
    transform = function(new) sweep(sweep(new, 2, m, "-"), 2, s, "/")
  )
}
