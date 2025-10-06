choose_features <- function(data, response, max_features,
                                     method = c("random","mutual","cor")) {
  method <- match.arg(method)
  predictors <- setdiff(names(data), response)

  if (length(predictors) <= max_features) return(predictors)

  if (method == "random") {
    set.seed(123)  # Consider making seed configurable
    return(sample(predictors, max_features))
  }

  if (method == "mutual") {
    # Check if FSelectorRcpp is available
    if (!requireNamespace("FSelectorRcpp", quietly = TRUE)) {
      warning("FSelectorRcpp not available, falling back to correlation method")
      method <- "cor"
    } else {
      tryCatch({
        scores <- FSelectorRcpp::information_gain(
          reformulate(predictors, response), data
        )
        return(head(scores[order(-scores$importance), "attributes"], max_features))
      }, error = function(e) {
        warning("Mutual information calculation failed, using correlation: ", e$message)
        method <<- "cor"
      })
    }
  }

  if (method == "cor") {
    y <- data[[response]]
    if (is.character(y)) y <- as.numeric(factor(y))
    if (is.factor(y)) y <- as.numeric(y)

    cor_vals <- sapply(predictors, function(p) {
      x <- data[[p]]
      if (!is.numeric(x)) {
        # Handle categorical variables
        if (is.factor(x) || is.character(x)) {
          # Use eta-squared (ANOVA) for categorical vs numeric
          tryCatch({
            model <- lm(y ~ x)
            anova_result <- anova(model)
            sqrt(anova_result$`Sum Sq`[1] / sum(anova_result$`Sum Sq`))
          }, error = function(e) NA_real_)
        } else {
          NA_real_
        }
      } else {
        suppressWarnings(abs(cor(x, y, use = "complete.obs")))
      }
    })

    cor_vals <- cor_vals[!is.na(cor_vals)]
    if (length(cor_vals) == 0) {
      warning("No valid features for correlation, using random selection")
      set.seed(123)
      return(sample(predictors, min(max_features, length(predictors))))
    }

    selected <- names(sort(cor_vals, decreasing = TRUE))[1:min(max_features, length(cor_vals))]
    return(selected)
  }
}
