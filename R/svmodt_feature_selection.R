library(FSelectorRcpp)

scale_node <- function(df) {
  mu <- sapply(df, mean, na.rm = TRUE)
  sdv <- sapply(df, sd, na.rm = TRUE)
  sdv[sdv == 0] <- 1
  scaled <- sweep(sweep(df, 2, mu), 2, sdv, "/")
  list(train = scaled, center = mu, scale = sdv)
}

apply_scaler <- function(df, scaler) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())

  # Use the transform function provided in scaler
  scaled <- scaler$transform(df)

  # Ensure result is a data.frame
  as.data.frame(scaled)
}

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
    scores <- FSelectorRcpp::information_gain(reformulate(predictors, response), data)
    topk <- head(scores[order(-scores$importance), "attributes"], max_features)
    return(topk)
  }

  if (method == "cor") {
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
    return(names(sort(cor_vals, decreasing = TRUE))[1:max_features])
  }
}
