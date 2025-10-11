calculate_node_class_weights <- function(y, class_weights = "none",
                                         custom_class_weights = NULL,
                                         verbose = FALSE) {
  class_weights <- match.arg(class_weights, c("none", "balanced", "balanced_subsample", "custom"))

  # Get class distribution at current node
  class_counts <- table(y)
  class_names <- names(class_counts)

  if (class_weights == "none") {
    if (verbose) cat("Class weights: none (equal weights)\n")
    return(NULL)
  }

  if (class_weights == "custom") {
    if (is.null(custom_class_weights)) {
      warning("class_weights='custom' but no custom_class_weights provided. Using 'none' instead.")
      return(NULL)
    }

    # Validate custom weights
    if (!all(class_names %in% names(custom_class_weights))) {
      warning("Custom weights missing some classes. Using 'none' instead.")
      return(NULL)
    }

    weights <- custom_class_weights[class_names]
    weights <- pmin(weights, 10)

    if (verbose) {
      cat("Class weights (custom):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }
    return(weights)
  }

  if (class_weights == "balanced") {
    n_samples <- length(y)
    n_classes <- length(class_counts)
    weights <- n_samples / (n_classes * as.numeric(class_counts))
    names(weights) <- class_names

    weights <- pmin(weights, 10)

    if (verbose) {
      cat("Class weights (balanced):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }
    return(weights)
  }

  if (class_weights == "balanced_subsample") {
    weights <- 1 / as.numeric(class_counts)
    names(weights) <- class_names
    weights <- weights * (length(class_counts) / sum(weights))

    weights <- pmin(weights, 10)

    if (verbose) {
      cat("Class weights (balanced_subsample):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }
    return(weights)
  }
}

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
