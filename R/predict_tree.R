predict_tree <- function(node, input, return_probs = FALSE) {
  # Handle batch prediction
  if (is.data.frame(input)) {
    return(
      apply(input, 1, function(row) {
        predict_tree(node, as.list(row), return_probs = return_probs)
      })
    )
  }

  # If leaf node
  if (!is.null(node$prediction)) {
    if (return_probs) {
      return(node$probs)
    } else {
      return(node$prediction)
    }
  }

  feature_value <- input[[node$split_feature]]
  if (is.null(feature_value)) {
    stop(paste("Missing feature:", node$split_feature))
  }

  if (feature_value <= node$split_value) {
    return(predict_tree(node$left, input, return_probs))
  } else {
    return(predict_tree(node$right, input, return_probs))
  }
}
