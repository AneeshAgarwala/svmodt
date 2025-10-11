calculate_impurity <- function(y, metric = c("entropy", "gini")) {
  metric <- match.arg(metric)
  p <- prop.table(table(y))
  p <- p[p > 0]  # Remove zeros for numerical stability

  if (metric == "entropy") {
    return(-sum(p * log2(p)))
  } else {
    return(1 - sum(p^2))
  }
}

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
