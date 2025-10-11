validate_svm_split_params <- function(data, response, max_depth, min_samples,
                                      max_features, feature_method, class_weights) {

  # Data validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  if (nrow(data) == 0) {
    stop("'data' cannot be empty")
  }

  # Response validation
  if (!is.character(response) || length(response) != 1) {
    stop("'response' must be a single character string")
  }

  if (!response %in% names(data)) {
    stop("Response variable '", response, "' not found in data")
  }

  # Numeric parameter validation
  if (!is.numeric(max_depth) || max_depth < 1) {
    stop("'max_depth' must be a positive integer")
  }

  if (!is.numeric(min_samples) || min_samples < 1) {
    stop("'min_samples' must be a positive integer")
  }

  if (!is.null(max_features)) {
    if (!is.numeric(max_features) || max_features < 1) {
      stop("'max_features' must be NULL or a positive integer")
    }
  }

  # Method validation
  valid_methods <- c("random", "mutual", "cor")
  if (!all(feature_method %in% valid_methods)) {
    stop("'feature_method' must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Class weights validation
  valid_weights <- c("none", "balanced", "balanced_subsample", "custom")
  if (!all(class_weights %in% valid_weights)) {
    stop("'class_weights' must be one of: ", paste(valid_weights, collapse = ", "))
  }

  TRUE
}
