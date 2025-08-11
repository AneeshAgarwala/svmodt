## Different Choices/Consideration for Feature Selection


## Gini Impurity for Variable Selection
gini <- function(labels) {
  counts <- tabulate(factor(labels))
  p <- counts / sum(counts)
  1 - sum(p * p)
}


## Entropy for Information Gain
entropy <- function(labels) {
  p <- prop.table(table(labels))
  -sum(p[p > 0] * log2(p[p > 0]))
}



## Information Gain Criteria for Variable Selection
info_gain <- function(feature, target) {
  total_entropy <- entropy(target)
  feature_levels <- unique(feature)

  weighted_entropy <- sum(
    sapply(feature_levels, function(level) {
      subset <- target[feature == level]
      (length(subset) / length(target)) * entropy(subset)
    })
  )

  ig <- total_entropy - weighted_entropy
  return(ig)
}


## Gain Ratio - Treats bias of Info Gain towards features with diverse set of values

# Quantifies how broadly and evenly the dataset is split by the feature
intrinsic_info <- function(feature) {
  p <- prop.table(table(feature))
  -sum(p[p > 0] * log2(p[p > 0]))
}



gain_ratio <- function(target, feature) {
  ig <- info_gain(feature = feature, target = target)
  si <- intrinsic_info(feature)
  if (si == 0) return(0)

  ig / si
}



## Wrapper Function for Feature Selection Criteria
feature_selector <- function(target, features, criteria_type = c("gini", "info_gain", "gain_ratio")){
  if (criteria_type == "gini"){
    scores <- sapply(features, gini)
    return(names(sort(scores, decreasing = TRUE)[1]))
  }
  else if(criteria_type == "info_gain"){
    scores <- sapply(features, function(feature) {
      info_gain(feature, target)
    })
    return(names(sort(scores, decreasing = TRUE)[1]))
  }
  else if(criteria_type == "gain_ratio"){
    scores <- sapply(features, function(feature) {
      gain_ratio(feature, target)
    })
    return(names(sort(scores, decreasing = TRUE)[1]))
  }
  else{
    stop("Please Select a Valid Criteria!")
  }
}
