select_best_ovr_split <- function(impurities_list, tie_break = "first") {
  # Extract impurities
  impurities <- sapply(impurities_list, function(x)
    if (!is.null(x$impurity) && !is.na(x$impurity)) x$impurity else Inf)

  # Find minimum
  min_impurity <- min(impurities[is.finite(impurities)])

  if (!is.finite(min_impurity)) {
    return(NULL)
  }

  # Find all classes with minimum impurity (for tie-breaking)
  best_classes <- names(impurities)[impurities == min_impurity]

  if (length(best_classes) == 1) {
    return(best_classes[1])
  }

  # Tie-breaking
  switch(tie_break,
         "first" = best_classes[1],  # Choose first in order
         "last" = best_classes[length(best_classes)],
         "random" = sample(best_classes, 1),
         best_classes[1]  # Default to first
  )
}
