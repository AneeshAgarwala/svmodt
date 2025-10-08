calculate_node_class_weights <- function(y, class_weights_method, custom_weights, verbose) {
  class_weights_method <- match.arg(class_weights_method, c("none", "balanced", "balanced_subsample", "custom"))

  # Get class distribution at this node
  class_counts <- table(y)
  class_names <- names(class_counts)

  if (class_weights_method == "none") {
    # No weighting - return NULL (equal weights)
    if (verbose) cat("Class weights: none (equal weights)\n")
    return(NULL)
  }

  if (class_weights_method == "custom") {
    # Use user-provided custom weights
    if (is.null(custom_weights)) {
      warning("class_weights='custom' but no custom_class_weights provided. Using 'none' instead.")
      return(NULL)
    } else {
      # Validate custom weights
      if (!all(class_names %in% names(custom_weights))) {
        warning("Custom weights missing some classes. Using 'none' instead.")
        return(NULL)
      } else {
        weights <- custom_weights[class_names]
        if (verbose) {
          cat("Class weights (custom):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
        }
        return(weights)
      }
    }
  }

  if (class_weights_method == "balanced") {
    # Balanced: inversely proportional to class frequencies
    # weight = n_samples / (n_classes * class_count)
    n_samples <- length(y)
    n_classes <- length(class_counts)

    weights <- n_samples / (n_classes * as.numeric(class_counts))
    names(weights) <- class_names

    if (verbose) {
      cat("Class weights (balanced):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }

    return(weights)
  }

  if (class_weights_method == "balanced_subsample") {
    # Balanced subsample: inversely proportional but normalized to sum to n_classes
    weights <- 1 / as.numeric(class_counts)
    names(weights) <- class_names

    # Normalize so sum = n_classes
    weights <- weights * (length(class_counts) / sum(weights))

    if (verbose) {
      cat("Class weights (balanced_subsample):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }

    return(weights)
  }
}

# Keep get_all_classes as is
get_all_classes <- function(tree) {
  if (tree$is_leaf) {
    return(names(tree$class_prob))
  }
  classes <- character(0)
  if (!is.null(tree$left)) {
    classes <- c(classes, get_all_classes(tree$left))
  }
  if (!is.null(tree$right)) {
    classes <- c(classes, get_all_classes(tree$right))
  }
  return(unique(classes))
}
