generate_tree <- function(target, features, criteria_type = c("gini", "info_gain", "gain_ratio"), ig_metric = c("gini", "entropy"),
                          depth = 1, max_depth = 5, p_stop = 0.3, all_levels = NULL) {

  if (is.null(all_levels)) {
    all_levels <- levels(factor(target))  # get all classes from original data (first call)
  }

  if (missing(ig_metric)){
    ig_metric <- NULL
  }

  if (criteria_type %in% c("gini", "gain_ratio")){
    ig_metric <- NULL
  }

  # Stopping condition
  if (depth >= max_depth || runif(1) < p_stop || length(unique(target)) == 1) {
    probs <- class_probabilities(target, all_levels)
    prediction <- names(probs)[which.max(probs)]
    return(list(prediction = prediction, probs = probs))
  }

  # Select best feature using selected criteria
  fvs <- feature_selector(target = target, features = features, criteria_type = criteria_type, ig_metric = ig_metric)
  split_feature <- fvs$feature
  split_value <- fvs$split

  # Edge case: No valid split found
  if (is.null(split_feature) || is.null(split_value)) {
    probs <- class_probabilities(target, all_levels)
    prediction <- names(probs)[which.max(probs)]
    return(list(prediction = prediction, probs = probs))
  }

  split_vals <- features[[split_feature]]

  # Split data depending on feature type
  if (is.numeric(split_vals)) {
    left_idx <- split_vals <= split_value
    right_idx <- !left_idx
  } else {
    # For categorical, left is those equal to split_value, right is rest
    left_idx <- split_vals == split_value
    right_idx <- split_vals != split_value
  }

  # Recursive calls
  left_subtree <- generate_tree(
    target = target[left_idx],
    features = features[left_idx, , drop = FALSE],
    criteria_type = criteria_type,
    depth = depth + 1,
    max_depth = max_depth,
    p_stop = p_stop,
    all_levels = all_levels,
    ig_metric = ig_metric
  )

  right_subtree <- generate_tree(
    target = target[right_idx],
    features = features[right_idx, , drop = FALSE],
    criteria_type = criteria_type,
    depth = depth + 1,
    max_depth = max_depth,
    p_stop = p_stop,
    all_levels = all_levels,
    ig_metric = ig_metric
  )

  list(
    split_feature = split_feature,
    split_value = split_value,
    left = left_subtree,
    right = right_subtree
  )
}


class_probabilities <- function(vec, all_levels) {
  f <- factor(vec, levels = all_levels)
  counts <- tabulate(f, nbins = length(all_levels))
  probs <- counts / sum(counts)
  names(probs) <- all_levels
  return(probs)
}
