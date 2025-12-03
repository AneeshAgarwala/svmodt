#' Calculate Information Gain Using SVM-based Splits
#'
#' Computes the information gain achieved by splitting data using a linear SVM
#' trained on a subset of features. The SVM's decision values determine the split,
#' and information gain is calculated based on the resulting partitions.
#'
#' @param feature_subset Character vector of feature names to use for the SVM split.
#' @param data A data frame containing predictors and the response variable.
#' @param response Character string specifying the response variable name.
#' @param metric Impurity measure for information gain calculation. One of:
#'   \itemize{
#'     \item \code{"entropy"} – entropy-based information gain (default).
#'     \item \code{"gini"} – Gini impurity-based information gain.
#'   }
#' @param verbose Logical; if \code{TRUE}, prints diagnostic information.
#'
#' @return Numeric value representing the information gain achieved by the SVM split.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Fits a linear SVM using the specified feature subset.
#'   \item Extracts decision values (distances from the hyperplane).
#'   \item Creates a binary split: samples with negative decision values go left,
#'         positive values go right.
#'   \item Calculates information gain using the \code{\link{info_gain}} function.
#' }
#'
#' The SVM split creates an oblique (non-axis-aligned) partition, potentially
#' capturing more complex decision boundaries than single-feature splits.
#'
#' @keywords internal
svm_info_gain <- function(feature_subset, data, response,
                          metric = c("entropy", "gini"),
                          verbose = FALSE) {
  metric <- match.arg(metric)

  # Validate inputs
  if (length(feature_subset) == 0) {
    warning("Empty feature subset provided")
    return(0)
  }

  if (!all(feature_subset %in% names(data))) {
    warning("Some features in subset not found in data")
    return(0)
  }

  if (!response %in% names(data)) {
    stop("Response variable '", response, "' not found in data")
  }

  # Extract data for SVM
  X <- data[, feature_subset, drop = FALSE]
  y <- data[[response]]

  if (verbose) {
    cat("Computing SVM info gain with features:",
        paste(feature_subset, collapse = ", "), "\n")
  }

  # Fit linear SVM
  tryCatch({
    model <- e1071::svm(
      x = X, y = y,
      kernel = "linear",
      scale = TRUE,
      decision.values = TRUE,
      probability = FALSE
    )

    # Get decision values
    pred <- predict(model, X, decision.values = TRUE)
    distances <- attr(pred, "decision.values")

    if (is.null(distances)) {
      warning("SVM did not return decision values")
      return(0)
    }

    # Convert to vector if matrix
    if (is.matrix(distances)) {
      distances <- distances[, 1]
    }

    # Create binary split based on decision values
    # Left: negative distances, Right: positive distances
    split_feature <- factor(
      ifelse(distances < 0, "left", "right"),
      levels = c("left", "right")
    )

    # Calculate information gain
    ig <- info_gain(split_feature, y, metric = metric)

    if (verbose) {
      cat("  Information gain:", round(ig, 4), "\n")
    }

    return(ig)

  }, error = function(e) {
    if (verbose) {
      warning("SVM info gain calculation failed: ", e$message)
    }
    return(0)
  })
}


#' Evaluate Multiple Random Feature Subsets Using SVM Information Gain
#'
#' Generates and evaluates multiple random feature subsets, ranking them by
#' the information gain achieved through SVM-based splits.
#'
#' @param data A data frame containing predictors and the response variable.
#' @param predictors Character vector of available predictor names.
#' @param response Character string specifying the response variable name.
#' @param n_subsets Integer; number of random feature subsets to evaluate.
#' @param subset_size Integer; number of features in each subset.
#' @param metric Impurity measure for information gain. One of \code{"entropy"} or \code{"gini"}.
#' @param verbose Logical; if \code{TRUE}, prints evaluation progress.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{features}{List column containing character vectors of feature names.}
#'   \item{info_gain}{Numeric vector of information gain values.}
#' }
#' The data frame is sorted in descending order by information gain.
#'
#' @details
#' This function randomly samples \code{n_subsets} different combinations of
#' \code{subset_size} features from the predictor pool, evaluates each subset
#' using \code{\link{svm_info_gain}}, and returns them ranked by performance.
#'
#' If \code{subset_size} is greater than the number of available predictors,
#' it is automatically reduced to match the predictor count.
#'
#' @keywords internal
evaluate_random_subsets <- function(data, predictors, response,
                                    n_subsets = 5, subset_size = 4,
                                    metric = c("entropy", "gini"),
                                    verbose = FALSE) {
  metric <- match.arg(metric)

  # Validate inputs
  if (length(predictors) == 0) {
    warning("No predictors provided")
    return(data.frame(features = list(), info_gain = numeric(0)))
  }

  # Adjust subset_size if necessary
  subset_size <- min(subset_size, length(predictors))

  if (verbose) {
    cat("Evaluating", n_subsets, "random subsets of size", subset_size, "\n")
  }

  # Generate random feature subsets
  feature_subsets <- vector("list", n_subsets)
  for (i in seq_len(n_subsets)) {
    feature_subsets[[i]] <- sample(predictors, subset_size, replace = FALSE)
  }

  # Evaluate each subset
  info_gains <- numeric(n_subsets)
  for (i in seq_len(n_subsets)) {
    if (verbose) {
      cat("  Evaluating subset", i, "of", n_subsets, "\n")
    }
    info_gains[i] <- svm_info_gain(
      feature_subsets[[i]], data, response, metric, verbose = FALSE
    )
  }

  # Create results data frame
  results <- data.frame(
    info_gain = info_gains,
    stringsAsFactors = FALSE
  )
  results$features <- feature_subsets

  # Sort by information gain (descending)
  results <- results[order(-results$info_gain), ]
  rownames(results) <- NULL

  if (verbose) {
    cat("Best subset info gain:", round(max(info_gains), 4), "\n")
  }

  return(results)
}
