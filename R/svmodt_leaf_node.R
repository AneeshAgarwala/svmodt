leaf_node <- function(y, n, features = character(0), scaler = NULL) {
  list(
    is_leaf   = TRUE,
    prediction= names(which.max(table(y))),
    n         = n,
    features  = features,
    scaler    = scaler
  )
}
