#' Dynamically determine the number of features to consider at a node
#'
#' Computes the number of features to be used for splitting at a given tree depth
#' based on the specified strategy. Supports constant, decreasing, and random
#' feature selection strategies.
#'
#' @param data A data frame containing the predictor variables and the response variable.
#' @param response A character string specifying the name of the response variable
#'   to exclude from the feature set.
#' @param base_max_features Integer; the base number of features to consider.
#'   If `NULL`, all available features (excluding the response) are used.
#' @param depth Integer; the current depth of the node in the tree (used for
#'   depth-dependent strategies).
#' @param strategy Character string specifying how to determine the number of
#'   features. One of:
#'   \itemize{
#'     \item `"constant"` – always use `base_max_features` (default).
#'     \item `"decrease"` – exponentially decrease the number of features with depth.
#'     \item `"random"` – randomly select the number of features within a range.
#'   }
#' @param decrease_rate Numeric; factor (0–1] controlling how fast the number of
#'   features decreases with depth when `strategy = "decrease"`. Default is 0.8.
#' @param random_range Numeric vector of length 2 specifying the lower and upper
#'   bounds (as proportions of total features) for random selection when
#'   `strategy = "random"`. Default is `c(0.3, 1.0)`.
#' @param verbose Logical; if `TRUE`, prints details about the chosen strategy
#'   and resulting feature count.
#'
#' @return Integer; the number of features to consider at the current node.
#'   The value is always constrained between 1 and the total number of available features.
#'
#' @details
#' This function helps control model complexity and randomness by varying
#' the number of features used at each split.
#'
#' Input parameters are validated to ensure sensible defaults. The result is capped
#' to avoid exceeding the total number of available features.
#' @keywords internal
calculate_dynamic_max_features <- function(data, response, base_max_features, depth,
                                           strategy = "constant",
                                           decrease_rate = 0.8,
                                           random_range = c(0.3, 1.0),
                                           verbose = FALSE) {
  total_features <- length(setdiff(names(data), response))

  # If no base_max_features specified, use all features
  if (is.null(base_max_features)) {
    base_max_features <- total_features
  }

  # FIXED: Validate inputs
  if (base_max_features < 1) base_max_features <- 1
  if (decrease_rate <= 0 || decrease_rate > 1) decrease_rate <- 0.8
  if (length(random_range) != 2 || random_range[1] >= random_range[2]) {
    random_range <- c(0.3, 1.0)
  }

  strategy <- match.arg(strategy, c("constant", "random", "decrease"))

  result <- switch(strategy,
    "constant" = base_max_features,
    "decrease" = {
      decreased <- round(base_max_features * (decrease_rate^(depth - 1)))
      max(1, min(decreased, total_features))
    },
    "random" = {
      min_features <- max(1, round(total_features * random_range[1]))
      max_features <- min(total_features, round(total_features * random_range[2]))
      if (min_features >= max_features) {
        min_features
      } else {
        sample(min_features:max_features, 1)
      }
    }
  )

  result <- min(result, total_features)

  if (verbose && strategy != "constant") {
    cat(
      "Strategy:", strategy, "- Base:", base_max_features,
      "-> Current:", result, "(depth", depth, ")\n"
    )
  }

  return(result)
}
