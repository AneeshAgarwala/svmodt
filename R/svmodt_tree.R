library(kernlab)

svm_split <- function(data, response, depth = 1, max_depth = 3,
                      max_features = NULL, feature_select = "random",
                      criterion = "entropy", min_samples = 5, ...) {

  y <- data[[response]]
  y_table <- table(y)
  major_class_prop <- max(y_table / sum(y_table))

  # Stopping criteria
  if (depth > max_depth || nrow(data) < min_samples || length(unique(y)) == 1 ||
      major_class_prop >= 0.95) {
    return(list(
      is_leaf = TRUE,
      prediction = names(which.max(y_table)),
      n = nrow(data),
      class_dist = y_table
    ))
  }

  # Feature selection
  if (!is.null(max_features)) {
    features <- choose_features(data, response, max_features, feature_select)
  } else {
    features <- setdiff(names(data), response)
  }

  # Scale features
  scaler <- scale_node(data[features], data[features])
  scaled <- cbind(data[response], scaler$train)
  names(scaled)[1] <- response

  # Train SVM
  model <- ksvm(reformulate(features, response), data = scaled, kernel = "vanilladot", prob.model = TRUE, ...)

  # Compute decision values
  dec <- predict(model, scaled, type = "decision")
  if (is.matrix(dec)) {
    best_col <- best_column(dec, scaled[[response]])
    vals <- dec[, best_col]
  } else {
    vals <- dec
  }

  left_idx  <- which(vals > 0)
  right_idx <- which(vals <= 0)

  # If children too small, stop
  if (length(left_idx) < min_samples || length(right_idx) < min_samples) {
    return(list(
      is_leaf = TRUE,
      prediction = names(which.max(y_table)),
      n = nrow(data),
      class_dist = y_table
    ))
  }

  # Recursive split
  left_child  <- svm_split(data[left_idx, , drop = FALSE], response, depth + 1,
                           max_depth, max_features, feature_select, criterion, min_samples, ...)
  right_child <- svm_split(data[right_idx, , drop = FALSE], response, depth + 1,
                           max_depth, max_features, feature_select, criterion, min_samples, ...)

  return(list(
    is_leaf = FALSE,
    model = model,
    features = features,
    left = left_child,
    right = right_child,
    depth = depth,
    n = nrow(data),
    class_dist = y_table
  ))
}

iris_tree <- svm_split(iris, "Species", max_depth = 2, max_features = 2)
