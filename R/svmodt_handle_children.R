handle_small_children <- function(left_idx, right_idx, min_samples,
                                  data, response, depth, max_depth,
                                  max_features, feature_method,
                                  max_features_strategy, max_features_decrease_rate,
                                  max_features_random_range,
                                  penalize_used_features, feature_penalty_weight,
                                  used_features,
                                  class_weights, custom_class_weights,
                                  features, scaler, all_classes, verbose, ...) {

  # Both children too small
  if (length(left_idx) < min_samples && length(right_idx) < min_samples) {
    if (verbose) cat("Stopping: both child nodes too small\n")
    return(list(
      stop = TRUE,
      node = leaf_node(data[[response]], nrow(data), all_classes, features, scaler)
    ))
  }

  # Left child too small
  if (length(left_idx) < min_samples) {
    if (verbose) cat("Left child too small, only creating right child\n")

    right_child <- svm_split(
      data[right_idx, , drop = FALSE], response,
      depth + 1, max_depth, min_samples,
      max_features, feature_method,
      max_features_strategy, max_features_decrease_rate, max_features_random_range,
      penalize_used_features, feature_penalty_weight, used_features,
      class_weights, custom_class_weights,
      verbose = verbose, all_classes = all_classes, ...
    )

    return(list(
      stop = FALSE,
      node = list(
        is_leaf = FALSE, model = NULL,
        features = features, scaler = scaler,
        best_col = 1, left = NULL, right = right_child,
        depth = depth, n = nrow(data)
      )
    ))
  }

  # Right child too small
  if (length(right_idx) < min_samples) {
    if (verbose) cat("Right child too small, only creating left child\n")

    left_child <- svm_split(
      data[left_idx, , drop = FALSE], response,
      depth + 1, max_depth, min_samples,
      max_features, feature_method,
      max_features_strategy, max_features_decrease_rate, max_features_random_range,
      penalize_used_features, feature_penalty_weight, used_features,
      class_weights, custom_class_weights,
      verbose = verbose, all_classes = all_classes, ...
    )

    return(list(
      stop = FALSE,
      node = list(
        is_leaf = FALSE, model = NULL,
        features = features, scaler = scaler,
        best_col = 1, left = left_child, right = NULL,
        depth = depth, n = nrow(data)
      )
    ))
  }

  list(stop = FALSE, node = NULL)
}
