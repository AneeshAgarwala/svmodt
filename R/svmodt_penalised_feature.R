choose_features_with_penalty <- function(data, response, max_features,
                                         method = c("random", "mutual", "cor"),
                                         penalize_used = FALSE,
                                         penalty_weight = 0.5,
                                         used_features = character(0),
                                         verbose = FALSE) {
  method <- match.arg(method)
  predictors <- setdiff(names(data), response)

  if (length(predictors) <= max_features) return(predictors)

  # FIXED: Validate and cap penalty weight
  penalty_weight <- max(0, min(penalty_weight, 0.99))

  if (!penalize_used || length(used_features) == 0) {
    return(choose_features(data, response, max_features, method))
  }

  if (verbose) {
    cat("Applying feature penalty (weight =", penalty_weight,
        ") to:", paste(used_features, collapse = ", "), "\n")
  }

  if (method == "random") {
    available_features <- intersect(predictors, names(data))
    weights <- rep(1, length(available_features))
    names(weights) <- available_features

    used_idx <- which(available_features %in% used_features)
    if (length(used_idx) > 0) {
      weights[used_idx] <- weights[used_idx] * (1 - penalty_weight)
    }

    selected <- sample(available_features, max_features, prob = weights, replace = FALSE)
    return(selected)
  }

  if (method == "mutual") {
    if (!requireNamespace("FSelectorRcpp", quietly = TRUE)) {
      warning("FSelectorRcpp not available, falling back to correlation with penalty")
      method <- "cor"
    } else {
      tryCatch({
        scores <- FSelectorRcpp::information_gain(
          reformulate(predictors, response), data
        )

        penalty_idx <- which(scores$attributes %in% used_features)
        if (length(penalty_idx) > 0) {
          scores$importance[penalty_idx] <- scores$importance[penalty_idx] * (1 - penalty_weight)
        }

        return(head(scores[order(-scores$importance), "attributes"], max_features))
      }, error = function(e) {
        warning("Mutual information with penalty failed, using correlation: ", e$message)
        method <- "cor"
      })
    }
  }

  if (method == "cor") {
    # FIXED: Use extracted helper function
    cor_vals <- calculate_feature_associations(data, response, predictors)

    if (length(cor_vals) == 0) {
      warning("No valid features for correlation, using random selection with penalty")
      return(choose_features_with_penalty(data, response, max_features, "random",
                                          penalize_used, penalty_weight, used_features, verbose))
    }

    penalty_idx <- which(names(cor_vals) %in% used_features)
    if (length(penalty_idx) > 0) {
      cor_vals[penalty_idx] <- cor_vals[penalty_idx] * (1 - penalty_weight)
      if (verbose) {
        cat("Penalized features:", paste(names(cor_vals)[penalty_idx], collapse = ", "), "\n")
      }
    }

    selected <- names(sort(cor_vals, decreasing = TRUE))[1:max_features]
    return(selected)
  }
}
