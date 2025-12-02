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
                                min_samples = 5, kernel = "linear",
                                impurity_measure = "entropy",
                                cost = 1, verbose = FALSE,
                                all_classes = NULL,
                                class_order_method = "natural",
                                tie_break_method = "first", ...) {

  # Initialize all_classes if NULL
  if (is.null(all_classes)) {
    all_classes <- get_consistent_class_order(data[[response]], class_order_method)
  }

  if (verbose) {
    cat("\n--- STree Node at depth", depth, "---\n")
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

  # Get unique classes at this node (maintain order)
  present_classes <- intersect(all_classes, unique(as.character(y)))
  k <- length(present_classes)

  if (verbose) cat("Number of classes at node:", k, "\n")

  # Prepare features
  features <- setdiff(names(data), response)
  X <- data[features]

  # Binary case
  if (k == 2) {
    if (verbose) cat("Binary classification case\n")

    result <- stree_fit_binary_svm(
      X, factor(y), kernel, verbose = verbose,
      use_scaling = TRUE, cost = cost, ...
    )

    if (is.null(result$model)) {
      return(leaf_node(y, n, all_classes))
    }

    left_idx <- result$left_idx
    right_idx <- result$right_idx

    if (length(left_idx) == 0 || length(right_idx) == 0 ||
        length(left_idx) < min_samples || length(right_idx) < min_samples) {
      return(leaf_node(y, n, all_classes))
    }

    # Recursive calls
    left_child <- stree_split(
      data[left_idx, ], response, depth + 1, max_depth, min_samples,
      kernel, impurity_measure, cost, verbose, all_classes,
      class_order_method, tie_break_method, ...
    )

    right_child <- stree_split(
      data[right_idx, ], response, depth + 1, max_depth, min_samples,
      kernel, impurity_measure, cost, verbose, all_classes,
      class_order_method, tie_break_method, ...
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

  # Multiclass case
  if (verbose) cat("Multi-class case: trying", k, "one-vs-rest splits\n")

  impurity_func <- if (impurity_measure == "entropy") entropy else gini
  impurities_list <- list()

  for (target_class in present_classes) {
    y_binary <- factor(
      ifelse(y == target_class, "positive", "negative"),
      levels = c("positive", "negative")
    )

    if (verbose) cat("  Trying:", target_class, "vs rest\n")

    result <- stree_fit_binary_svm(
      X, y_binary, kernel, verbose = FALSE,
      use_scaling = TRUE, cost = cost, ...
    )

    if (is.null(result$model)) {
      impurities_list[[target_class]] <- list(impurity = NA)
      next
    }

    left_idx <- result$left_idx
    right_idx <- result$right_idx

    if (length(left_idx) == 0 || length(right_idx) == 0) {
      impurities_list[[target_class]] <- list(impurity = NA)
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

    impurities_list[[target_class]] <- list(
      impurity = weighted_impurity,
      model = result$model,
      left_idx = left_idx,
      right_idx = right_idx,
      scaling_params = result$scaling_params,
      used_features = result$used_features
    )
  }

  # Select best split with tie-breaking
  best_class <- select_best_ovr_split(impurities_list, tie_break_method)

  if (is.null(best_class)) {
    if (verbose) cat("No valid split found, creating leaf\n")
    return(leaf_node(y, n, all_classes))
  }

  best_result <- impurities_list[[best_class]]

  if (verbose) {
    cat("Best split: class", best_class, "vs rest\n")
    cat("Best impurity:", round(best_result$impurity, 4), "\n")
  }

  # Check child sizes
  if (length(best_result$left_idx) < min_samples ||
      length(best_result$right_idx) < min_samples) {
    return(leaf_node(y, n, all_classes))
  }

  # Recursive calls
  left_child <- stree_split(
    data[best_result$left_idx, ], response, depth + 1, max_depth, min_samples,
    kernel, impurity_measure, cost, verbose, all_classes,
    class_order_method, tie_break_method, ...
  )

  right_child <- stree_split(
    data[best_result$right_idx, ], response, depth + 1, max_depth, min_samples,
    kernel, impurity_measure, cost, verbose, all_classes,
    class_order_method, tie_break_method, ...
  )

  return(list(
    is_leaf = FALSE,
    model = best_result$model,
    features = best_result$used_features,
    scaling_params = best_result$scaling_params,
    hyperplane_class = best_class,
    left = left_child,
    right = right_child,
    depth = depth,
    n = n,
    impurity = best_result$impurity,
    kernel = kernel
  ))
}

# Helper functions
entropy <- function(y) {
  if (length(y) == 0) return(0)
  probs <- table(y) / length(y)
  probs <- probs[probs > 0]
  -sum(probs * log2(probs))
}

gini <- function(y) {
  if (length(y) == 0) return(0)
  probs <- table(y) / length(y)
  1 - sum(probs^2)
}

leaf_node <- function(y, n, all_classes) {
  freq <- table(factor(y, levels = all_classes))
  probs <- freq / n
  prediction <- names(which.max(freq))

  list(
    is_leaf = TRUE,
    prediction = prediction,
    class_prob = as.vector(probs),
    class_names = all_classes,
    n = n
  )
}
