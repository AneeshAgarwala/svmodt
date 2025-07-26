generate_tree <- function(target, features, criteria_type = c("gini", "info_gain", "gain_ratio"),
                          depth = 1, max_depth = 5, p_stop = 0.3) {

  # Helper: Safe majority class prediction
  majority_prediction <- function(vec) {
    f <- factor(vec)
    if (length(f) == 0 || all(is.na(f))) {
      return(NA)
    } else {
      counts <- tabulate(f)
      if (length(counts) == 0) {
        return(NA)
      } else {
        return(levels(f)[which.max(counts)])
      }
    }
  }

  # Stopping condition
  if (depth >= max_depth || runif(1) < p_stop || length(unique(target)) == 1) {
    prediction <- majority_prediction(target)
    return(list(prediction = prediction))
  }

  # Select best feature using selected criteria
  split_feature <- feature_selector(target = target, features = features, criteria_type = criteria_type)

  # Select a split value randomly within the range of the feature
  split_vals <- features[[split_feature]]
  split_value <- round(runif(1, min(split_vals), max(split_vals)), 2)

  # Split data based on selected feature and split value
  left_idx <- split_vals <= split_value
  right_idx <- !left_idx

  # Edge case: if either side is empty, stop recursion
  if (all(!left_idx) || all(!right_idx)) {
    prediction <- safe_majority_prediction(target)
    return(list(prediction = prediction))
  }

  # Recursive calls on left and right subtrees
  left_subtree <- generate_tree(
    target = target[left_idx],
    features = features[left_idx, , drop = FALSE],
    criteria_type = criteria_type,
    depth = depth + 1,
    max_depth = max_depth,
    p_stop = p_stop
  )

  right_subtree <- generate_tree(
    target = target[right_idx],
    features = features[right_idx, , drop = FALSE],
    criteria_type = criteria_type,
    depth = depth + 1,
    max_depth = max_depth,
    p_stop = p_stop
  )

  # Return node structure
  list(
    split_feature = split_feature,
    split_value = split_value,
    left = left_subtree,
    right = right_subtree
  )
}
