choose_features <- function(data, response, max_features,
                            method = c("random","mutual","cor")) {
  method <- match.arg(method)
  predictors <- setdiff(names(data), response)
  if (length(predictors) <= max_features) return(predictors)

  if (method == "random") {
    set.seed(123)
    return(sample(predictors, max_features))
  }

  if (method == "mutual") {
    scores <- FSelectorRcpp::information_gain(
      reformulate(predictors, response), data
    )
    head(scores[order(-scores$importance), "attributes"], max_features)
  } else { # correlation
    y <- data[[response]]
    if (is.character(y)) y <- as.numeric(factor(y))
    if (is.factor(y))   y <- as.numeric(y)
    if (!is.numeric(y)) stop("Response must be numeric for correlation.")
    cor_vals <- sapply(predictors, function(p) {
      x <- data[[p]]
      if (!is.numeric(x)) return(NA_real_)
      suppressWarnings(abs(cor(x, y, use = "complete.obs")))
    })
    cor_vals <- cor_vals[!is.na(cor_vals)]
    names(sort(cor_vals, decreasing = TRUE))[1:max_features]
  }
}
