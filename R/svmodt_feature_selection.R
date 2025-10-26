#' @title Calculate feature associations with a response variable
#' @description
#' Computes the association strength between each predictor and the response variable.
#' For numeric predictors, the absolute Pearson correlation is used. For categorical
#' predictors, association is estimated using an ANOVA-based pseudo-\eqn{R^2} measure.
#'
#' @param data A data frame containing the response and predictor variables.
#' @param response A string specifying the response variable name.
#' @param predictors A character vector of predictor names to evaluate.
#'
#' @return A named numeric vector of association values (0 to 1) for each predictor.
#'
#' @details
#' - **Numeric predictors:** Computed using the absolute Pearson correlation.
#' - **Categorical predictors:** Uses the square root of the ratio of between-group
#'   sum of squares to total sum of squares from an ANOVA model.
#'
#' @keywords internal
calculate_feature_associations <- function(data, response, predictors) {
  y <- data[[response]]
  if (is.character(y)) y <- as.numeric(factor(y))
  if (is.factor(y)) y <- as.numeric(y)

  assoc_vals <- sapply(predictors, function(p) {
    x <- data[[p]]
    if (!is.numeric(x)) {
      # Handle categorical variables
      if (is.factor(x) || is.character(x)) {
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

  assoc_vals[!is.na(assoc_vals)]
}

#' Select a subset of features based on correlation, mutual information, or randomness
#'
#' Chooses up to a specified number of features from a dataset using one of three methods:
#' random sampling, correlation with the response, or mutual information ranking.
#'
#' @param data A data frame containing the response and predictor variables.
#' @param response A string specifying the response variable name.
#' @param max_features Integer specifying the maximum number of features to select.
#' @param method Selection strategy. One of:
#'   \itemize{
#'     \item \code{"random"} – randomly selects features.
#'     \item \code{"mutual"} – ranks features by mutual information with the response
#'       (requires \pkg{FSelectorRcpp}).
#'     \item \code{"cor"} – ranks features by absolute correlation with the response.
#'   }
#'
#' @return A character vector of selected feature names.
#'
#' @details
#' - If the number of predictors is less than or equal to \code{max_features}, all are returned.
#' - If \code{method = "mutual"} and \pkg{FSelectorRcpp} is not installed or fails,
#'   the function gracefully falls back to the correlation-based method.
#' - The correlation method internally calls
#'   \code{\link{calculate_feature_associations}}.
#'
#' @examples
#' df <- data.frame(
#'   y = rnorm(100),
#'   x1 = rnorm(100),
#'   x2 = runif(100),
#'   x3 = sample(letters[1:3], 100, replace = TRUE)
#' )
#' choose_features(df, "y", max_features = 2, method = "cor")
#' choose_features(df, "y", max_features = 2, method = "random")
#'
#' @seealso \code{\link{calculate_feature_associations}}
#' @keywords internal
choose_features <- function(data, response, max_features,
                            method = c("random", "mutual", "cor")) {
  method <- match.arg(method)
  predictors <- setdiff(names(data), response)

  if (length(predictors) <= max_features) return(predictors)

  if (method == "random") {
    return(sample(predictors, max_features))
  }

  if (method == "mutual") {
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
        method <- "cor"
      })
    }
  }

  if (method == "cor") {
    cor_vals <- calculate_feature_associations(data, response, predictors)

    if (length(cor_vals) == 0) {
      warning("No valid features for correlation, using random selection")
      return(sample(predictors, min(max_features, length(predictors))))
    }

    selected <- names(sort(cor_vals, decreasing = TRUE))[1:min(max_features, length(cor_vals))]
    return(selected)
  }
}
