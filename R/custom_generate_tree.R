generate_tree <- function(target, features,
                          criteria_type = c("gini", "info_gain", "gain_ratio"),
                          ig_metric = c("gini", "entropy"),
                          depth = 1, max_depth = 10,
                          all_levels = NULL, alpha = 0.0) {
  criteria_type <- match.arg(criteria_type)
  ig_metric <- match.arg(ig_metric)

  if (is.null(all_levels)) {
    all_levels <- levels(factor(target))
  }

  # node stats used for both leaf and internal nodes
  probs <- class_probabilities(target, all_levels)
  prediction <- names(probs)[which.max(probs)]
  n <- length(target)

  # stopping criteria: max depth or pure node
  if (depth >= max_depth || length(unique(target)) <= 1) {
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  }

  # choose best split
  fs <- feature_selector(target, features, criteria_type, ig_metric)
  split_feature <- fs$feature
  split_value <- fs$split

  # if no valid split, return leaf
  if (is.null(split_feature) || is.null(split_value) || is.infinite(fs$score)) {
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  }

  fvec <- features[[split_feature]]
  if (is.numeric(fvec)) {
    left_idx <- fvec <= split_value
    right_idx <- !left_idx
  } else {
    left_idx <- fvec == split_value
    right_idx <- fvec != split_value
  }

  # guard: if split creates empty child, revert to leaf
  if (sum(left_idx) == 0 || sum(right_idx) == 0) {
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  }

  # grow children
  left_subtree <- generate_tree(
    target = target[left_idx],
    features = features[left_idx, , drop = FALSE],
    criteria_type = criteria_type,
    ig_metric = ig_metric,
    depth = depth + 1,
    max_depth = max_depth,
    all_levels = all_levels,
    alpha = alpha
  )

  right_subtree <- generate_tree(
    target = target[right_idx],
    features = features[right_idx, , drop = FALSE],
    criteria_type = criteria_type,
    ig_metric = ig_metric,
    depth = depth + 1,
    max_depth = max_depth,
    all_levels = all_levels,
    alpha = alpha
  )

  # form internal node
  node <- list(
    split_feature = split_feature,
    split_value = split_value,
    left = left_subtree,
    right = right_subtree,
    probs = probs,
    prediction = prediction,
    n = n
  )

  # --- Integrated cost–complexity pruning decision ---
  # Cost of subtree
  cost_subtree <- subtree_error(node) + alpha * count_leaves(node)
  # Cost if we collapse to a leaf here
  cost_leaf <- (1 - max(probs)) + alpha * 1

  if (cost_leaf <= cost_subtree) {
    # prune: return leaf
    return(list(
      prediction = prediction,
      probs = probs,
      n = n
    ))
  } else {
    return(node)
  }
}
