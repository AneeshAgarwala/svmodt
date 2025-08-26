predict_tree <- function(node, input, return_probs = FALSE) {
  # Batch prediction for data.frame input
  if (is.data.frame(input)) {
    preds <- lapply(seq_len(nrow(input)), function(i) {
      predict_tree(node, as.list(input[i, , drop = FALSE]), return_probs = return_probs)
    })

    if (return_probs) {
      preds_matrix <- do.call(rbind, preds)
      rownames(preds_matrix) <- NULL
      return(preds_matrix)
    } else {
      return(unlist(preds))
    }
  }

  # Leaf node
  if (is.null(node$split_feature)) {
    if (return_probs) {
      return(node$probs)
    } else {
      return(node$prediction)
    }
  }

  # Internal node: route by split
  feature_value <- input[[node$split_feature]]
  if (is.null(feature_value)) {
    warning(paste("Missing feature:", node$split_feature, "- returning NA"))
    if (return_probs) {
      return(rep(NA_real_, length(node$probs)))
    } else {
      return(NA)
    }
  }

  if (is.numeric(feature_value) && is.numeric(node$split_value)) {
    if (feature_value <= node$split_value) {
      return(predict_tree(node$left, input, return_probs))
    } else {
      return(predict_tree(node$right, input, return_probs))
    }
  } else {
    if (feature_value == node$split_value) {
      return(predict_tree(node$left, input, return_probs))
    } else {
      return(predict_tree(node$right, input, return_probs))
    }
  }
}
