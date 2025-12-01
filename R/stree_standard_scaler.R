#' @title Standard Scaler for R (matches sklearn)
#' @description
#' Standardizes features by removing the mean and scaling to unit variance.
#' This matches Python's sklearn.preprocessing.StandardScaler behavior.
#'
#' @param X A data frame or matrix to scale
#' @param center Logical; if TRUE, center the data (subtract mean)
#' @param scale_var Logical; if TRUE, scale to unit variance
#' @param return_params Logical; if TRUE, return scaling parameters for later use
#' @param params List of pre-computed scaling parameters (mean and sd) from training data
#'
#' @return Scaled data or list with scaled data and parameters
#'
#' @export
standard_scaler <- function(X, center = TRUE, scale_var = TRUE,
                            return_params = FALSE, params = NULL) {

  # Convert to data frame if matrix
  if (is.matrix(X)) {
    X <- as.data.frame(X)
  }

  # Identify numeric columns
  numeric_cols <- sapply(X, is.numeric)

  if (sum(numeric_cols) == 0) {
    warning("No numeric columns to scale")
    if (return_params) {
      return(list(data = X, params = list(means = NULL, sds = NULL)))
    } else {
      return(X)
    }
  }

  X_scaled <- X

  # If params provided (for test data), use those
  if (!is.null(params)) {
    for (col in names(params$means)) {
      if (col %in% names(X_scaled)) {
        if (center) {
          X_scaled[[col]] <- X_scaled[[col]] - params$means[[col]]
        }
        if (scale_var && params$sds[[col]] > 0) {
          X_scaled[[col]] <- X_scaled[[col]] / params$sds[[col]]
        }
      }
    }
    return(X_scaled)
  }

  # Otherwise, compute parameters from data
  means <- list()
  sds <- list()

  for (col in names(X_scaled)[numeric_cols]) {
    col_data <- X_scaled[[col]]

    # Remove NA for calculation
    col_data_clean <- col_data[!is.na(col_data)]

    if (length(col_data_clean) == 0) {
      means[[col]] <- 0
      sds[[col]] <- 1
      next
    }

    # Calculate mean
    col_mean <- if (center) mean(col_data_clean, na.rm = TRUE) else 0
    means[[col]] <- col_mean

    # Calculate standard deviation (using n, not n-1 like R's default)
    # This matches sklearn's behavior
    if (scale_var) {
      if (length(col_data_clean) > 1) {
        # sklearn uses population std (divides by n, not n-1)
        col_sd <- sqrt(sum((col_data_clean - col_mean)^2) / length(col_data_clean))
      } else {
        col_sd <- 1
      }

      # Avoid division by zero
      if (col_sd == 0 || is.na(col_sd)) {
        col_sd <- 1
      }
    } else {
      col_sd <- 1
    }
    sds[[col]] <- col_sd

    # Apply transformation
    if (center) {
      X_scaled[[col]] <- X_scaled[[col]] - col_mean
    }
    if (scale_var) {
      X_scaled[[col]] <- X_scaled[[col]] / col_sd
    }
  }

  if (return_params) {
    return(list(
      data = X_scaled,
      params = list(means = means, sds = sds)
    ))
  } else {
    return(X_scaled)
  }
}
