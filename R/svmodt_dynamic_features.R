calculate_dynamic_max_features <- function(data, response, base_max_features, depth,
                                           strategy, decrease_rate, random_range, verbose) {

  total_features <- length(setdiff(names(data), response))

  # If no base_max_features specified, use all features
  if (is.null(base_max_features)) {
    base_max_features <- total_features
  }

  strategy <- match.arg(strategy, c("constant", "random", "decrease"))

  result <- switch(strategy,
                   "constant" = base_max_features,

                   "decrease" = {
                     # Decrease by rate each level: base * rate^(depth-1)
                     decreased <- round(base_max_features * (decrease_rate ^ (depth - 1)))
                     max(1, min(decreased, total_features))
                   },

                   "random" = {
                     # Random between min and max proportion of total features
                     min_features <- max(1, round(total_features * random_range[1]))
                     max_features <- min(total_features, round(total_features * random_range[2]))
                     if (min_features >= max_features) min_features else sample(min_features:max_features, 1)
                   }
  )

  # Ensure we don't exceed available features
  result <- min(result, total_features)

  if (verbose && strategy != "constant") {
    cat("Strategy:", strategy, "- Base:", base_max_features,
        "-> Current:", result, "(depth", depth, ")\n")
  }

  return(result)
}
