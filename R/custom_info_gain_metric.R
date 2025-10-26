info_gain_metric <- function(feature, target, metric = c("entropy", "gini")) {
  metric <- match.arg(metric)

  # Choose impurity function
  impurity_fun <- switch(metric,
                         "entropy" = entropy,
                         "gini" = gini)

  # Parent impurity
  total_impurity <- impurity_fun(target)

  # Unique feature levels
  feature_levels <- unique(feature)

  # Weighted impurity after split
  weighted_impurity <- sum(
    sapply(feature_levels, function(level) {
      subset <- target[feature == level]
      (length(subset) / length(target)) * impurity_fun(subset)
    })
  )

  # Information gain = impurity reduction
  ig <- total_impurity - weighted_impurity
  return(ig)
}
