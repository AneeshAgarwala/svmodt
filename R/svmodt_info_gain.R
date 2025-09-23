entropy <- function(y) {
  p <- prop.table(table(y))
  -sum(p * log2(p + 1e-9))
}

info_gain <- function(feature, target, metric = c("entropy","gini")) {
  metric <- match.arg(metric)
  impurity_fun <- if (metric == "entropy") {
    function(y) { p <- prop.table(table(y)); -sum(p[p > 0] * log2(p[p > 0])) }
  } else {
    function(y) { p <- prop.table(table(y)); 1 - sum(p^2) }
  }

  parent <- impurity_fun(target)
  weighted_child <- 0
  for (lv in levels(factor(feature))) {
    idx <- feature == lv
    weighted_child <- weighted_child +
      (sum(idx) / length(target)) * impurity_fun(target[idx])
  }
  parent - weighted_child
}
