#' Calculate node impurity
#'
#' Computes the impurity of a node using either Gini impurity or entropy.
#'
#' @param y A vector of class labels for the node.
#' @param method A string specifying the impurity measure: either "gini" or "entropy".
#'
#' @return A numeric value representing the impurity of the node.
#'
#' @details
#' If \code{method = "gini"}, the impurity is calculated as:
#' \deqn{
#'   G = 1 - \sum_i p_i^2
#' }{
#'   G = 1 - sum_i p_i^2
#' }
#' where \eqn{p_i} is the proportion of samples in class \eqn{i} in the node.
#'
#' If \code{method = "entropy"}, the impurity is calculated as:
#' \deqn{
#'   H = - \sum_i p_i \log(p_i)
#' }{
#'   H = - sum_i p_i * log(p_i)
#' }
#' @keywords internal
calculate_impurity <- function(y, method = c("gini", "entropy")) {
  method <- match.arg(method)
  p <- table(y) / length(y)
  if (method == "gini") {
    return(1 - sum(p^2))
  } else {
    p <- p[p > 0]
    return(-sum(p * log2(p)))
  }
}


#' Calculate Information Gain for a Feature Split
#'
#' Computes the reduction in impurity (information gain) when splitting a target
#' variable by a categorical feature.
#'
#' @param feature A vector representing the splitting feature (categorical or factor).
#' @param target A vector of class labels for the target variable.
#' @param metric The impurity measure to use: either "entropy" or "gini".
#'
#' @return A numeric value representing the information gain.
#'
#' @details
#' Information gain is computed as:
#' \deqn{
#'   IG = H(parent) - \sum_{v \in Values} \frac{n_v}{n} H(child_v)
#' }{
#'   IG = H(parent) - sum_v (n_v / n) * H(child_v)
#' }
#' where:
#' \itemize{
#'   \item \eqn{H(parent)} is the impurity of the original target vector,
#'   \item \eqn{H(child_v)} is the impurity of the subset of target where feature = v,
#'   \item \eqn{n_v} is the number of samples where feature = v,
#'   \item \eqn{n} is the total number of samples.
#' }
#'
#' @keywords internal
info_gain <- function(feature, target, metric = c("entropy", "gini")) {
  metric <- match.arg(metric)

  parent_impurity <- calculate_impurity(target, metric)

  weighted_child_impurity <- 0
  for (lv in levels(factor(feature))) {
    idx <- feature == lv
    if (sum(idx) > 0) {
      weight <- sum(idx) / length(target)
      child_impurity <- calculate_impurity(target[idx], metric)
      weighted_child_impurity <- weighted_child_impurity + weight * child_impurity
    }
  }

  parent_impurity - weighted_child_impurity
}
