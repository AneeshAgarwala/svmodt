#' @title Select features with optional penalty for previously used features
#' @description
#' Internal helper function to select a subset of features while optionally penalizing
#' features that have been used in ancestor nodes. Supports random selection,
#' mutual information, or correlation-based ranking.
#'
#' @param data A data frame containing predictors and the response.
#' @param response Name of the response variable.
#' @param max_features Maximum number of features to select.
#' @param method Feature selection method; one of \code{"random"}, \code{"mutual"}, or \code{"cor"}.
#' @param penalize_used Logical; if \code{TRUE}, previously used features are penalized.
#' @param penalty_weight Numeric (0–0.99); fraction by which to reduce the score/weight of used features.
#' @param used_features Character vector of features previously used in the tree.
#' @param verbose Logical; if \code{TRUE}, prints information about penalties applied.
#'
#' @return Character vector of selected feature names.
#'
#' @details
#' - Penalized features have their selection weight or score reduced by multiplying by \code{(1 - penalty_weight)}.
#' - For \code{method = "random"}, the penalty reduces the probability of sampling a feature.
#' - For \code{method = "mutual"} or \code{"cor"}, the penalty reduces feature importance or correlation.
#' - If no valid features are available for correlation, the function falls back to random selection with penalty.
#' - Ensures that no feature is entirely excluded; \code{penalty_weight} is capped below 1.
#'
#' @seealso \code{\link{choose_features}}, \code{\link{calculate_feature_associations}}
#' @keywords internal
choose_features_with_penalty <- function(data, response, max_features,
                                         method = c("random", "mutual", "cor"),
                                         penalize_used = FALSE,
                                         penalty_weight = 0.5,
                                         used_features = character(0),
                                         verbose = FALSE) {
  method <- match.arg(method)
  predictors <- setdiff(names(data), response)

  if (length(predictors) <= max_features) {
    return(predictors)
  }

  penalty_weight <- max(0, min(penalty_weight, 0.99))

  if (!penalize_used || length(used_features) == 0) {
    return(choose_features(data, response, max_features, method))
  }

  if (verbose) {
    cat(
      "Applying feature penalty (weight =", penalty_weight,
      ") to:", paste(used_features, collapse = ", "), "\n"
    )
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
      tryCatch(
        {
          scores <- FSelectorRcpp::information_gain(
            reformulate(predictors, response), data
          )

          penalty_idx <- which(scores$attributes %in% used_features)
          if (length(penalty_idx) > 0) {
            scores$importance[penalty_idx] <- scores$importance[penalty_idx] * (1 - penalty_weight)
          }

          return(head(scores[order(-scores$importance), "attributes"], max_features))
        },
        error = function(e) {
          warning("Mutual information with penalty failed, using correlation: ", e$message)
          method <- "cor"
        }
      )
    }
  }

  if (method == "cor") {
    cor_vals <- calculate_feature_associations(data, response, predictors)

    if (length(cor_vals) == 0) {
      warning("No valid features for correlation, using random selection with penalty")
      return(choose_features_with_penalty(
        data, response, max_features, "random",
        penalize_used, penalty_weight, used_features, verbose
      ))
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
