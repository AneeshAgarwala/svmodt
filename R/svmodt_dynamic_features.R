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
                     decreased <- round(base_max_features * (decrease_rate ^ (depth - 1)))
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
    cat("Strategy:", strategy, "- Base:", base_max_features,
        "-> Current:", result, "(depth", depth, ")\n")
  }

  return(result)
}
