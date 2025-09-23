handle_small_children <- function(left_idx, right_idx, min_samples,
                                  data, response, depth, max_depth,
                                  max_features, feature_method,
                                  features, scaler, verbose, ...) {
  # both small
  if (length(left_idx) < min_samples && length(right_idx) < min_samples) {
    if (verbose) cat("Stopping: both child nodes too small\n")
    return(list(stop = TRUE,
                node = leaf_node(data[[response]], nrow(data),
                                 features, scaler)))
  }
  # one side small
  if (length(left_idx) < min_samples) {
    if (verbose) cat("Left child too small, only right child\n")
    right_child <- svm_split(data[right_idx, , drop = FALSE], response,
                             depth + 1, max_depth, min_samples,
                             max_features, feature_method,
                             verbose = verbose, ...)
    return(list(stop = FALSE,
                node = list(is_leaf = FALSE, model = NULL,
                            features = features, scaler = scaler,
                            best_col = 1, left = NULL,
                            right = right_child,
                            depth = depth, n = nrow(data))))
  }
  if (length(right_idx) < min_samples) {
    if (verbose) cat("Right child too small, only left child\n")
    left_child <- svm_split(data[left_idx, , drop = FALSE], response,
                            depth + 1, max_depth, min_samples,
                            max_features, feature_method,
                            verbose = verbose, ...)
    return(list(stop = FALSE,
                node = list(is_leaf = FALSE, model = NULL,
                            features = features, scaler = scaler,
                            best_col = 1, left = left_child,
                            right = NULL,
                            depth = depth, n = nrow(data))))
  }
  list(stop = FALSE, node = NULL)
}
