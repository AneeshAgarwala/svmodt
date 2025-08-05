generate_tree <- function(target, features, criteria_type = c("gini", "info_gain", "gain_ratio"),
                          depth = 1, max_depth = 5, p_stop = 0.3) {

  # Stopping condition
  if (depth >= max_depth || runif(1) < p_stop || length(unique(target)) == 1) {
    probs <- class_probabilities(target)
    prediction <- names(probs)[which.max(probs)]
    return(list(prediction = prediction, probs = probs))
  }

  # Select best feature using selected criteria
  split_feature <- feature_selector(target = target, features = features, criteria_type = criteria_type)

  # Get values of selected feature
  split_vals <- features[[split_feature]]

  # Final split value
  split_value <- calculate_split_value(target, split_vals, criteria_type)

  # Edge case: No valid split found
  if (is.na(split_value)) {
    probs <- class_probabilities(target)
    prediction <- names(probs)[which.max(probs)]
    return(list(prediction = prediction, probs = probs))
  }

  # Split data
  left_idx <- split_vals <= split_value
  right_idx <- !left_idx

  # Recursive calls
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

  # Return current node
  list(
    split_feature = split_feature,
    split_value = split_value,
    left = left_subtree,
    right = right_subtree
  )
}
