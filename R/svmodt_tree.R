#' Build an Oblique Decision Tree Using SVM Splits
#'
#' Constructs a decision tree where each internal node uses a Support Vector
#' Machine (SVM) to determine the split. Supports dynamic feature selection,
#' feature penalization, scaling, and class weighting.
#'
#' @param data A data frame containing predictors and the response variable.
#' @param response Character string specifying the response column in `data`.
#'   All other columns are treated as predictors.
#' @param depth Integer indicating the current recursion depth (used internally; default is 1).
#' @param max_depth Maximum depth of the tree.
#' @param min_samples Minimum number of samples required to attempt a split.
#' @param max_features Maximum number of features to consider at each split.
#' @param feature_method Feature selection method at each node. One of:
#'   \itemize{
#'     \item `"random"`: randomly select features,
#'     \item `"mutual"`: select based on mutual information with the response,
#'     \item `"cor"`: select based on correlation with the response.
#'   }
#' @param max_features_strategy Strategy to adjust the number of features per node:
#'   \itemize{
#'     \item `"constant"`: keep `max_features` constant,
#'     \item `"decrease"`: reduce features with depth,
#'     \item `"random"`: randomly vary number of features within a range.
#'   }
#' @param max_features_decrease_rate Numeric fraction for decreasing features if
#'   `max_features_strategy = "decrease"`.
#' @param max_features_random_range Numeric vector of length 2 specifying min and max
#'   fraction of features if `max_features_strategy = "random"`.
#' @param penalize_used_features Logical; if TRUE, features used in ancestor nodes
#'   are penalized to encourage diversity.
#' @param feature_penalty_weight Numeric (0–1) weight for penalizing previously used features.
#' @param used_features Character vector of features already used in ancestor nodes
#'   (used internally).
#' @param class_weights Character string specifying how to handle class imbalance. One of:
#'   \itemize{
#'     \item `"none"`: no weighting,
#'     \item `"balanced"`: weight classes inversely proportional to their frequency,
#'     \item `"balanced_subsample"`: weight per node based on local class distribution,
#'     \item `"custom"`: use `custom_class_weights`.
#'   }
#' @param custom_class_weights Optional named numeric vector specifying custom weights per class.
#' @param all_classes Optional character vector of all possible response classes (used internally).
#' @param verbose Logical; if TRUE, prints information about each node during tree construction.
#' @param ... Additional arguments passed to the underlying SVM fitting function.
#'
#' @return A nested list representing the decision tree. Each node contains:
#' \describe{
#'   \item{is_leaf}{Logical; TRUE if the node is a leaf.}
#'   \item{model}{Fitted SVM model at this node (for internal nodes).}
#'   \item{features}{Vector of features selected for this node.}
#'   \item{scaler}{Scaling information used at this node.}
#'   \item{left}{Left child node (decision value > 0).}
#'   \item{right}{Right child node (decision value ≤ 0).}
#'   \item{depth}{Depth of this node in the tree.}
#'   \item{n}{Number of samples at this node.}
#'   \item{max_features_used}{Number of features considered at this node.}
#'   \item{penalty_applied}{Logical; TRUE if feature penalization was applied.}
#'   \item{class_weights_used}{Class weights applied at this node.}
#' }
#'
#' @details
#' This function recursively splits the dataset using an SVM at each node. Splitting
#' stops when maximum depth is reached, the node contains fewer than `min_samples`,
#' or all samples belong to the same class. Features are scaled and selected dynamically
#' at each node, and previously used features can be penalized to promote diversity.
#' Class weighting schemes support handling imbalanced datasets. This approach allows
#' construction of an **oblique decision tree**, where splits are linear hyperplanes
#' rather than axis-aligned.
#'
#' @examples
#' data(wdbc)
#' tree <- svm_split(
#'   data = wdbc,
#'   response = "diagnosis",
#'   max_depth = 3,
#'   min_samples = 5,
#'   feature_method = "random",
#'   verbose = TRUE
#' )
#'
#' @export
svm_split_ovr <- function(data, response, depth = 1, max_depth = 3,
                          min_samples = 5, max_features = NULL,
                          feature_method = c("random", "mutual", "cor"),
                          impurity_measure = c("entropy", "gini"),
                          max_features_strategy = c("constant", "random", "decrease"),
                          max_features_decrease_rate = 0.8,
                          max_features_random_range = c(0.3, 1.0),
                          penalize_used_features = FALSE,
                          feature_penalty_weight = 0.5,
                          used_features = character(0),
                          class_weights = c("none", "balanced", "balanced_subsample", "custom"),
                          custom_class_weights = NULL,
                          verbose = FALSE,
                          all_classes = NULL, ...) {

  # Match arguments
  feature_method <- match.arg(feature_method)
  impurity_measure <- match.arg(impurity_measure)
  max_features_strategy <- match.arg(max_features_strategy)
  class_weights <- match.arg(class_weights)

  # Initialize all_classes if NULL
  if (is.null(all_classes)) {
    all_classes <- levels(factor(data[[response]]))
  }

  # Validate inputs
  if (!response %in% names(data)) {
    stop("Response variable '", response, "' not found in data")
  }

  if (verbose) {
    cat("\n--- OVR Node at depth", depth, "---\n")
    cat("Samples:", nrow(data), "\n")
    cat("Class distribution:\n")
    print(table(data[[response]]))
  }

  # Handle NA rows
  if (anyNA(data)) {
    if (verbose) cat("Warning: NA values detected! Stopping here.\n")
    return(leaf_node(data[[response]], nrow(data), all_classes))
  }

  y <- data[[response]]
  n <- nrow(data)

  # Stopping conditions
  if (depth > max_depth || length(unique(y)) == 1 || n < min_samples) {
    if (verbose) cat("Creating leaf node\n")
    return(leaf_node(y, n, all_classes))
  }

  # Get unique classes at this node
  present_classes <- unique(as.character(y))
  k <- length(present_classes)

  if (verbose) cat("Number of classes at node:", k, "\n")

  # Calculate dynamic max_features
  current_max_features <- calculate_dynamic_max_features(
    data, response, max_features, depth,
    max_features_strategy, max_features_decrease_rate,
    max_features_random_range, verbose
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
    return(leaf_node(y, n, all_classes))
  }

  if (verbose) {
    cat("Selected features:", paste(features, collapse = ", "), "\n")
  }

  # Scaling
  scaler <- scale_node(data[features])
  X_scaled <- scaler$train

  if (ncol(X_scaled) == 0) {
    if (verbose) cat("Stopping: all features are constant\n")
    return(leaf_node(y, n, all_classes, features, scaler))
  }

  # BINARY CASE: k = 2
  if (k == 2) {
    if (verbose) cat("Binary classification case\n")

    # Calculate class weights for binary case
    node_class_weights <- calculate_node_class_weights(
      y, class_weights, custom_class_weights, verbose
    )

    # Fit binary SVM
    model <- fit_svm_with_weights(X_scaled, y, node_class_weights, verbose, ...)

    if (is.null(model)) {
      return(leaf_node(y, n, all_classes, features, scaler))
    }

    # Get decision values
    dec <- attr(predict(model, X_scaled, decision.values = TRUE), "decision.values")
    decision_values <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)

    left_idx <- which(decision_values > 0)
    right_idx <- which(decision_values <= 0)

    if (length(left_idx) == 0 || length(right_idx) == 0) {
      return(leaf_node(y, n, all_classes, features, scaler))
    }

    # Check child sizes
    if (length(left_idx) < min_samples || length(right_idx) < min_samples) {
      child_check <- handle_small_children(
        left_idx, right_idx, min_samples,
        data, response, depth, max_depth,
        max_features, feature_method,
        max_features_strategy, max_features_decrease_rate,
        max_features_random_range,
        penalize_used_features, feature_penalty_weight, used_features,
        class_weights, custom_class_weights,
        features, scaler, all_classes, verbose, ...
      )

      if (child_check$stop) {
        return(child_check$node)
      }
      if (!is.null(child_check$node)) {
        child_check$node$model <- model
        return(child_check$node)
      }
    }

    # Update used features
    updated_used_features <- if (penalize_used_features) {
      unique(c(used_features, features))
    } else {
      used_features
    }

    # Recursive calls for binary case
    left_child <- svm_split_ovr(
      data[left_idx, , drop = FALSE], response,
      depth + 1, max_depth, min_samples,
      max_features, feature_method, impurity_measure,
      max_features_strategy, max_features_decrease_rate,
      max_features_random_range,
      penalize_used_features, feature_penalty_weight, updated_used_features,
      class_weights, custom_class_weights,
      verbose = verbose, all_classes = all_classes, ...
    )

    right_child <- svm_split_ovr(
      data[right_idx, , drop = FALSE], response,
      depth + 1, max_depth, min_samples,
      max_features, feature_method, impurity_measure,
      max_features_strategy, max_features_decrease_rate,
      max_features_random_range,
      penalize_used_features, feature_penalty_weight, updated_used_features,
      class_weights, custom_class_weights,
      verbose = verbose, all_classes = all_classes, ...
    )

    return(list(
      is_leaf = FALSE,
      model = model,
      features = features,
      scaler = scaler,
      hyperplane_class = NULL,  # Not applicable for binary
      best_col = 1,
      left = left_child,
      right = right_child,
      depth = depth,
      n = n,
      max_features_used = current_max_features,
      penalty_applied = penalize_used_features && length(used_features) > 0,
      class_weights_used = node_class_weights
    ))
  }

  # MULTICLASS CASE: k > 2
  # Try all k one-vs-rest cases
  if (verbose) cat("Multi-class case: trying", k, "one-vs-rest splits\n")

  best_impurity <- Inf
  best_model <- NULL
  best_left_idx <- NULL
  best_right_idx <- NULL
  best_class <- NULL
  best_class_weights <- NULL

  # Select impurity function
  impurity_func <- if (impurity_measure == "entropy") entropy else gini

  for (target_class in present_classes) {
    # Create binary labels: target_class vs rest
    y_binary <- factor(
      ifelse(y == target_class, "positive", "negative"),
      levels = c("positive", "negative")
    )

    if (verbose) cat("  Trying:", target_class, "vs rest\n")

    # Calculate class weights for this binary problem
    node_class_weights <- calculate_node_class_weights(
      y_binary, class_weights, custom_class_weights, verbose = FALSE
    )

    # Fit SVM for this one-vs-rest split
    model <- fit_svm_with_weights(X_scaled, y_binary, node_class_weights,
                                  verbose = FALSE, ...)

    if (is.null(model)) {
      next
    }

    # Get decision values
    dec <- attr(predict(model, X_scaled, decision.values = TRUE), "decision.values")
    decision_values <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)

    left_idx <- which(decision_values > 0)
    right_idx <- which(decision_values <= 0)

    # Skip if split creates empty partition
    if (length(left_idx) == 0 || length(right_idx) == 0) {
      next
    }

    # Calculate weighted impurity on ORIGINAL labels
    y_left <- y[left_idx]
    y_right <- y[right_idx]

    impurity_left <- impurity_func(y_left)
    impurity_right <- impurity_func(y_right)

    weighted_impurity <- (length(left_idx) / n) * impurity_left +
      (length(right_idx) / n) * impurity_right

    if (verbose) {
      cat("    Weighted impurity:", round(weighted_impurity, 4), "\n")
    }

    # Update best split if better
    if (weighted_impurity < best_impurity) {
      best_impurity <- weighted_impurity
      best_model <- model
      best_left_idx <- left_idx
      best_right_idx <- right_idx
      best_class <- target_class
      best_class_weights <- node_class_weights
    }
  }

  # Check if we found a valid split
  if (is.null(best_model)) {
    if (verbose) cat("No valid split found, creating leaf\n")
    return(leaf_node(y, n, all_classes, features, scaler))
  }

  if (verbose) {
    cat("Best split: class", best_class, "vs rest\n")
    cat("Best impurity:", round(best_impurity, 4), "\n")
  }

  # Check child sizes
  if (length(best_left_idx) < min_samples || length(best_right_idx) < min_samples) {
    child_check <- handle_small_children(
      best_left_idx, best_right_idx, min_samples,
      data, response, depth, max_depth,
      max_features, feature_method,
      max_features_strategy, max_features_decrease_rate,
      max_features_random_range,
      penalize_used_features, feature_penalty_weight, used_features,
      class_weights, custom_class_weights,
      features, scaler, all_classes, verbose, ...
    )

    if (child_check$stop) {
      return(child_check$node)
    }
    if (!is.null(child_check$node)) {
      child_check$node$model <- best_model
      child_check$node$hyperplane_class <- best_class
      return(child_check$node)
    }
  }

  # Update used features
  updated_used_features <- if (penalize_used_features) {
    unique(c(used_features, features))
  } else {
    used_features
  }

  # Recursive calls with best split
  left_child <- svm_split_ovr(
    data[best_left_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples,
    max_features, feature_method, impurity_measure,
    max_features_strategy, max_features_decrease_rate,
    max_features_random_range,
    penalize_used_features, feature_penalty_weight, updated_used_features,
    class_weights, custom_class_weights,
    verbose = verbose, all_classes = all_classes, ...
  )

  right_child <- svm_split_ovr(
    data[best_right_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples,
    max_features, feature_method, impurity_measure,
    max_features_strategy, max_features_decrease_rate,
    max_features_random_range,
    penalize_used_features, feature_penalty_weight, updated_used_features,
    class_weights, custom_class_weights,
    verbose = verbose, all_classes = all_classes, ...
  )

  return(list(
    is_leaf = FALSE,
    model = best_model,
    features = features,
    scaler = scaler,
    hyperplane_class = best_class,  # Store which class was selected
    best_col = 1,
    left = left_child,
    right = right_child,
    depth = depth,
    n = n,
    impurity = best_impurity,
    max_features_used = current_max_features,
    penalty_applied = penalize_used_features && length(used_features) > 0,
    class_weights_used = best_class_weights
  ))
}
