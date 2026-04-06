#' Calculate Gini Impurity
#'
#' Computes the Gini impurity for a vector of class labels.
#'
#' @param y A vector of class labels.
#' @return Numeric value representing Gini impurity (0 = pure, higher = more impure).
#' @keywords internal
gini <- function(y) {
  if (length(y) == 0) {
    return(0)
  }
  p <- table(y) / length(y)
  return(1 - sum(p^2))
}

#' Calculate Entropy
#'
#' Computes the entropy for a vector of class labels.
#'
#' @param y A vector of class labels.
#' @return Numeric value representing entropy (0 = pure, higher = more impure).
#' @keywords internal
entropy <- function(y) {
  if (length(y) == 0) {
    return(0)
  }
  p <- table(y) / length(y)
  p <- p[p > 0] # Remove zero probabilities to avoid log(0)
  return(-sum(p * log2(p)))
}
