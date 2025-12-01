#' @title Build STree: SVM Oblique Decision Tree with One-vs-Rest Multi-class Strategy
#' @description
#' Implements the STree algorithm that uses one-vs-rest strategy for multi-class
#' problems, selecting the best binary split based on impurity minimization.
#'
#' @param data A data frame containing predictors and response.
#' @param response Character string specifying the response column name.
#' @param depth Current recursion depth (internal use).
#' @param max_depth Maximum depth of the tree.
#' @param min_samples Minimum samples required to split a node.
#' @param kernel SVM kernel type: "linear", "polynomial", or "radial".
#' @param impurity_measure Impurity measure: "entropy" or "gini".
#' @param verbose Logical; if TRUE, prints node information.
#' @param all_classes Character vector of all possible classes (internal use).
#' @param ... Additional arguments passed to e1071::svm.
#'
#' @return A nested list representing the STree decision tree.
#'
#' @export
stree_split <- function(data, response, depth = 1, max_depth = 5,
                        min_samples = 5, kernel = c("linear", "polynomial", "radial"),
                        impurity_measure = c("entropy", "gini"),
                        verbose = FALSE, all_classes = NULL, max_features = NULL, ...) {

  kernel <- match.arg(kernel)
  impurity_measure <- match.arg(impurity_measure)

  # Initialize all_classes if NULL
  if (is.null(all_classes)) {
    all_classes <- levels(factor(data[[response]]))
  }

  # Validate inputs
  if (!response %in% names(data)) {
    stop("Response variable '", response, "' not found in data")
  }

  y <- data[[response]]
  n <- nrow(data)

  # Determine features
  feature_names <- setdiff(names(data), response)
  n_features <- length(feature_names)


  # Compute max_features based on user specification
  if (is.null(max_features)) {
    m_features <- n_features
  } else if (is.character(max_features)) {
    if (max_features %in% c("auto", "sqrt")) {
      m_features <- max(1, floor(sqrt(n_features)))
    } else if (max_features == "log2") {
      m_features <- max(1, floor(log2(n_features)))
    } else {
      stop("Invalid max_features string option")
    }
  } else if (is.numeric(max_features)) {
    if (max_features > 1) {
      m_features <- min(n_features, as.integer(max_features))
    } else if (max_features > 0 && max_features <= 1) {
      m_features <- max(1, as.integer(max_features * n_features))
    } else {
      stop("Invalid numeric max_features value")
    }
  } else {
    stop("Invalid max_features argument")
  }

  # Randomly sample selected features
  selected_features <- sample(feature_names, m_features)

  if (verbose) {
    cat("\n--- STree Node at depth", depth, "---\n")
    cat("Samples:", n, "\n")
    cat("Class distribution:\n")
    print(table(y))
  }

  # Stopping conditions
  if (depth >= max_depth || n < min_samples || length(unique(y)) == 1) {
    if (verbose) cat("Creating leaf node\n")
    return(stree_leaf_node(y, n, all_classes))
  }

  # Get unique classes in current node
  present_classes <- unique(as.character(y))
  k <- length(present_classes)

  if (verbose) cat("Number of classes at node:", k, "\n")

  # Prepare feature matrix (exclude response)
  feature_names <- setdiff(names(data), response)
  X <- data[, selected_features, drop = FALSE]

  # Binary case: k = 2
  if (k == 2) {
    if (verbose) cat("Binary classification case\n")

    result <- stree_fit_binary_svm(X, y, kernel, verbose, ...)

    if (is.null(result$model)) {
      return(stree_leaf_node(y, n, all_classes))
    }

    # Split data based on SVM decision
    left_idx <- result$left_idx
    right_idx <- result$right_idx

    if (length(left_idx) == 0 || length(right_idx) == 0) {
      return(stree_leaf_node(y, n, all_classes))
    }

    # Recursive calls
    left_child <- stree_split(
      data[left_idx, , drop = FALSE], response,
      depth + 1, max_depth, min_samples, kernel,
      impurity_measure, verbose, all_classes, max_features,...
    )

    right_child <- stree_split(
      data[right_idx, , drop = FALSE], response,
      depth + 1, max_depth, min_samples, kernel,
      impurity_measure, verbose, all_classes,  max_features,...
    )

    return(list(
      is_leaf = FALSE,
      model = result$model,
      features = result$used_features,
      scaling_params = result$scaling_params,
      hyperplane_class = NULL,
      left = left_child,
      right = right_child,
      depth = depth,
      n = n,
      kernel = kernel
    ))
  }

  # Multi-class case: k > 2
  # Try all k one-vs-rest cases
  if (verbose) cat("Multi-class case: trying", k, "one-vs-rest splits\n")

  best_impurity <- Inf
  best_model <- NULL
  best_left_idx <- NULL
  best_right_idx <- NULL
  best_class <- NULL
  best_scaling_params <- NULL

  impurity_func <- if (impurity_measure == "entropy") entropy else gini

  for (target_class in present_classes) {
    # Create binary labels: target_class vs rest
    y_binary <- factor(ifelse(y == target_class, "positive", "negative"),
                       levels = c("positive", "negative"))

    if (verbose) cat("  Trying:", target_class, "vs rest\n")

    # Fit SVM
    result <- stree_fit_binary_svm(X, y_binary, kernel, verbose = FALSE, ...)

    if (is.null(result$model)) {
      next
    }

    left_idx <- result$left_idx
    right_idx <- result$right_idx

    # Skip if split creates empty partition
    if (length(left_idx) == 0 || length(right_idx) == 0) {
      next
    }

    # Calculate weighted impurity
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
      best_model <- result$model
      best_left_idx <- left_idx
      best_right_idx <- right_idx
      best_class <- target_class
      best_used_features <- result$used_features
      best_scaling_params <- result$scaling_params
    }
  }

  # Check if we found a valid split
  if (is.null(best_model)) {
    if (verbose) cat("No valid split found, creating leaf\n")
    return(stree_leaf_node(y, n, all_classes))
  }

  if (verbose) {
    cat("Best split: class", best_class, "vs rest\n")
    cat("Best impurity:", round(best_impurity, 4), "\n")
  }

  # Recursive calls with best split
  left_child <- stree_split(
    data[best_left_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples, kernel,
    impurity_measure, verbose, all_classes,  max_features,...
  )

  right_child <- stree_split(
    data[best_right_idx, , drop = FALSE], response,
    depth + 1, max_depth, min_samples, kernel,
    impurity_measure, verbose, all_classes,  max_features,...
  )

  return(list(
    is_leaf = FALSE,
    model = best_model,
    features = best_used_features,
    scaling_params = best_scaling_params,  # NEW
    hyperplane_class = best_class,
    left = left_child,
    right = right_child,
    depth = depth,
    n = n,
    kernel = kernel,
    impurity = best_impurity
  ))
}
