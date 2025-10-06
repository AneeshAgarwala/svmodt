choose_features_with_penalty <- function(data, response, max_features, method,
                                         penalize_used, penalty_weight, used_features,
                                         verbose = FALSE) {

  method <- match.arg(method, c("random", "mutual", "cor"))
  predictors <- setdiff(names(data), response)

  if (length(predictors) <= max_features) return(predictors)

  if (!penalize_used || length(used_features) == 0) {
    # No penalty - use original method
    return(choose_features(data, response, max_features, method))
  }

  if (verbose) {
    cat("Applying feature penalty (weight =", penalty_weight,
        ") to:", paste(used_features, collapse = ", "), "\n")
  }

  if (method == "random") {
    # For random selection, reduce probability of selecting used features
    available_features <- intersect(predictors, names(data))

    # Create weights: lower for used features
    weights <- rep(1, length(available_features))
    names(weights) <- available_features

    used_idx <- which(available_features %in% used_features)
    if (length(used_idx) > 0) {
      weights[used_idx] <- weights[used_idx] * (1 - penalty_weight)
    }

    # Sample with weights
    set.seed(123)  # For reproducibility
    selected <- sample(available_features, max_features, prob = weights, replace = FALSE)
    return(selected)
  }

  # For mutual info and correlation, modify scores
  if (method == "mutual") {
    if (!requireNamespace("FSelectorRcpp", quietly = TRUE)) {
      warning("FSelectorRcpp not available, falling back to correlation with penalty")
      method <- "cor"
    } else {
      tryCatch({
        scores <- FSelectorRcpp::information_gain(
          reformulate(predictors, response), data
        )

        # Apply penalty to used features (cap penalty_weight at 0.99)
        capped_penalty <- min(penalty_weight, 0.99)
        penalty_idx <- which(scores$attributes %in% used_features)
        if (length(penalty_idx) > 0) {
          scores$importance[penalty_idx] <- scores$importance[penalty_idx] * (1 - capped_penalty)
        }

        return(head(scores[order(-scores$importance), "attributes"], max_features))
      }, error = function(e) {
        warning("Mutual information with penalty failed, using correlation: ", e$message)
        method <- "cor"
      })
    }
  }

  if (method == "cor") {
    y <- data[[response]]
    if (is.character(y)) y <- as.numeric(factor(y))
    if (is.factor(y)) y <- as.numeric(y)

    cor_vals <- sapply(predictors, function(p) {
      x <- data[[p]]
      if (!is.numeric(x)) return(NA_real_)
      suppressWarnings(abs(cor(x, y, use = "complete.obs")))
    })

    cor_vals <- cor_vals[!is.na(cor_vals)]

    # Apply penalty to used features (cap penalty_weight at 0.99)
    capped_penalty <- min(penalty_weight, 0.99)
    penalty_idx <- which(names(cor_vals) %in% used_features)
    if (length(penalty_idx) > 0) {
      cor_vals[penalty_idx] <- cor_vals[penalty_idx] * (1 - capped_penalty)

      if (verbose) {
        cat("Penalized features:", paste(names(cor_vals)[penalty_idx], collapse = ", "), "\n")
      }
    }

    selected <- names(sort(cor_vals, decreasing = TRUE))[1:max_features]
    return(selected)
  }
}
