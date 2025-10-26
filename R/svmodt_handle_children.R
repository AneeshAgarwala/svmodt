#' @title Handle small child nodes in tree splitting
#' @description
#' Internal helper function to handle situations where one or both child nodes
#' resulting from a split have fewer samples than \code{min_samples}. Depending on
#' which child is too small, it may stop splitting, create only one child, or
#' return a flag to continue normal processing.
#'
#' @param left_idx Indices of samples assigned to the left child.
#' @param right_idx Indices of samples assigned to the right child.
#' @param min_samples Minimum number of samples required for a node to be valid.
#' @param data The full dataset being split.
#' @param response Name of the response variable.
#' @param depth Current depth of the node.
#' @param max_depth Maximum allowed depth for the tree.
#' @param max_features Maximum number of features to consider at each split.
#' @param feature_method Feature selection method (e.g., "random", "cor", "mutual").
#' @param max_features_strategy Strategy for dynamic feature selection ("constant", "decrease", "random").
#' @param max_features_decrease_rate Numeric; factor controlling feature decrease with depth.
#' @param max_features_random_range Numeric vector of length 2 specifying min/max proportion for random features.
#' @param penalize_used_features Logical; whether to penalize previously used features.
#' @param feature_penalty_weight Numeric weight for penalizing used features.
#' @param used_features Character vector of features used in ancestor nodes.
#' @param class_weights Named numeric vector of class weights.
#' @param custom_class_weights Optional custom class weights.
#' @param features Character vector of features used at this node.
#' @param scaler Optional scaler applied to features at this node.
#' @param all_classes Character vector of all possible classes.
#' @param verbose Logical; if TRUE, prints messages for debugging.
#' @param ... Additional arguments passed to \code{svm_split}.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{stop} Logical; \code{TRUE} if splitting should stop at this node.
#'   \item \code{node} Either a leaf node object (if stopping) or a partially built
#'         internal node with only one child (if one child is too small).
#' }
#'
#' @details
#' - If both children are smaller than \code{min_samples}, a leaf node is created.
#' - If only one child is too small, the other child is recursively split.
#' - This function ensures that tree nodes respect the minimum sample requirement,
#'   avoiding invalid splits that could destabilize the SVM-based tree.
#'
#' @keywords internal
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
