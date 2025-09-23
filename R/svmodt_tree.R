svm_split <- function(data, response, depth = 1, max_depth = 3,
                      min_samples = 5, max_features = NULL,
                      feature_method = c("random", "mutual", "cor"),
                      verbose = FALSE, all_classes = NULL, ...) {


  if (is.null(all_classes)) {
    all_classes <- levels(factor(data[[response]]))
  }

  if (verbose) {
    cat("\n--- Node at depth", depth, "---\n")
    cat("Rows:", nrow(data), "\n")
    cat("Response distribution:\n")
    print(table(data[[response]]))
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

  # Feature selection
  features <- if (is.null(max_features)) setdiff(names(data), response)
  else choose_features(data, response, max_features, feature_method)
  if (length(features) == 0) {
    if (verbose) cat("Stopping: no usable features\n")
    return(leaf_node(y, nrow(data), all_classes))
  }
  if (verbose) cat("Using features:", paste(features, collapse = ", "), "\n")

  # Scaling
  scaler   <- scale_node(data[features])
  X_scaled <- scaler$train

  # Fit SVM
  model <- fit_svm(X_scaled, y, verbose, ...)
  if (is.null(model)) {
    return(leaf_node(y, nrow(data), all_classes, features, scaler))
  }

  # Decision boundary
  dec <- attr(predict(model, X_scaled, decision.values = TRUE), "decision.values")
  dec_vals <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)
  left_idx  <- which(dec_vals > 0)
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
                                       max_features, feature_method,
                                       features, scaler, verbose, ...)
  if (child_check$stop) return(child_check$node)
  if (!is.null(child_check$node)) {
    child_check$node$model <- model
    return(child_check$node)
  }

  # Recurse
  left  <- svm_split(data[left_idx, , drop = FALSE], response,
                     depth + 1, max_depth, min_samples,
                     max_features, feature_method,
                     verbose = verbose, all_classes = all_classes, ...)
  right <- svm_split(data[right_idx, , drop = FALSE], response,
                     depth + 1, max_depth, min_samples,
                     max_features, feature_method,
                     verbose = verbose, all_classes = all_classes, ...)

  list(
    is_leaf = FALSE,
    model   = model,
    features= features,
    scaler  = scaler,
    best_col= 1,
    left    = left,
    right   = right,
    depth   = depth,
    n       = nrow(data)
  )
}
