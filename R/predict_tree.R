predict_tree <- function(node, input) {
  if (!is.null(node$prediction)) {
    return(node$prediction)
  }

  feature_value <- input[[node$split_feature]]
  if (is.null(feature_value)) {
    stop(paste("Missing feature:", node$split_feature))
  }

  if (feature_value <= node$split_value) {
    return(predict_tree(node$left, input))
  } else {
    return(predict_tree(node$right, input))
  }
}
