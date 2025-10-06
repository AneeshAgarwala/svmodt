svm_split <- function(data, response, depth = 1, max_depth = 3,
                               min_samples = 5, max_features = NULL,
                               feature_method = c("random", "mutual", "cor"),

                               max_features_strategy = c("constant", "random", "decrease"),
                               max_features_decrease_rate = 0.8,  # Multiply by this each level
                               max_features_random_range = c(0.3, 1.0),  # Min/max proportion

                               penalize_used_features = FALSE,
                               feature_penalty_weight = 0.5,  # How much to penalize (0-1)
                               used_features = character(0),  # Features used in parent nodes

                               class_weights = c("none", "balanced", "balanced_subsample", "custom"),
                               custom_class_weights = NULL,  # Named vector if class_weights="custom"

                               verbose = FALSE, all_classes = NULL, ...) {

  if (is.null(all_classes)) {
    all_classes <- levels(factor(data[[response]]))
  }

  if (verbose) {
    cat("\n--- Node at depth", depth, "---\n")
    cat("Rows:", nrow(data), "\n")
    cat("Response distribution:\n")
    print(table(data[[response]]))
    if (length(used_features) > 0) {
      cat("Used features in ancestors:", paste(used_features, collapse = ", "), "\n")
    }
  }

  # Handle NA rows
  if (anyNA(data)) {
    if (verbose) cat("Warning: NA values detected! Stopping here.\n")
    return(leaf_node(data[[response]], nrow(data), all_classes))
  }

  y <- data[[response]]

  # Stopping rules
  if (depth > max_depth || length(unique(y)) == 1 || nrow(data) < min_samples) {
    if (verbose) cat("Stopping at depth", depth, "\n")
    return(leaf_node(y, nrow(data), all_classes))
  }

  # Calculate class weights for this node
  node_class_weights <- calculate_node_class_weights(
    y, class_weights, custom_class_weights, verbose
  )

  # FEATURE 1: Dynamic max_features calculation
  current_max_features <- calculate_dynamic_max_features(
    data, response, max_features, depth,
    max_features_strategy, max_features_decrease_rate, max_features_random_range,
    verbose
  )

  # Feature selection with penalties
  features <- if (is.null(current_max_features)) {
    setdiff(names(data), response)
  } else {
    choose_features_with_penalty(
      data, response, current_max_features, feature_method,
      penalize_used_features, feature_penalty_weight, used_features,
      verbose
    )
  }

  if (length(features) == 0) {
    if (verbose) cat("Stopping: no usable features\n")
    return(leaf_node(y, nrow(data), all_classes))
  }

  if (verbose) {
    cat("Dynamic max_features:", current_max_features, "\n")
    cat("Selected features:", paste(features, collapse = ", "), "\n")
  }

  # Scaling
  scaler <- scale_node(data[features])
  X_scaled <- scaler$train

  # Fit SVM with calculated class weights
  model <- fit_svm_with_weights(X_scaled, y, node_class_weights, verbose, ...)
  if (is.null(model)) {
    return(leaf_node(y, nrow(data), all_classes, features, scaler))
  }

  # Decision boundary
  dec <- attr(predict(model, X_scaled, decision.values = TRUE), "decision.values")
  dec_vals <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)
  left_idx <- which(dec_vals > 0)
  right_idx <- which(dec_vals <= 0)

  if (verbose) cat("Left size:", length(left_idx), "Right size:", length(right_idx), "\n")

  # No effective split
  if (length(left_idx) == 0 || length(right_idx) == 0) {
    if (verbose) cat("Stopping: ineffective split (all points on one side)\n")
    return(leaf_node(y, nrow(data), all_classes, features, scaler))
  }

  # Small child handling
  child_check <- handle_small_children(left_idx, right_idx, min_samples,
                                       data, response, depth, max_depth,
                                       current_max_features, feature_method,
                                       features, scaler, verbose, ...)
  if (child_check$stop) return(child_check$node)
  if (!is.null(child_check$node)) {
    child_check$node$model <- model
    return(child_check$node)
  }

  # Update used features for children (FEATURE 2: Feature penalty)
  updated_used_features <- if (penalize_used_features) {
    unique(c(used_features, features))
  } else {
    used_features
  }

  # Recurse with enhanced parameters
  left <- svm_split(
    data[left_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples,
    max_features, feature_method,
    max_features_strategy, max_features_decrease_rate, max_features_random_range,
    penalize_used_features, feature_penalty_weight, updated_used_features,
    class_weights, custom_class_weights,  # Pass class weights to children
    verbose = verbose, all_classes = all_classes, ...
  )

  right <- svm_split(
    data[right_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples,
    max_features, feature_method,
    max_features_strategy, max_features_decrease_rate, max_features_random_range,
    penalize_used_features, feature_penalty_weight, updated_used_features,
    class_weights, custom_class_weights,  # Pass class weights to children
    verbose = verbose, all_classes = all_classes, ...
  )

  list(
    is_leaf = FALSE,
    model = model,
    features = features,
    scaler = scaler,
    best_col = 1,
    left = left,
    right = right,
    depth = depth,
    n = nrow(data),
    max_features_used = current_max_features,  # Track for analysis
    penalty_applied = penalize_used_features && length(used_features) > 0,
    class_weights_used = node_class_weights  # Track class weights used
  )
}
