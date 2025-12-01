#' @title Predict with STree
#' @description
#' Makes predictions using an STree decision tree by traversing from root to leaf.
#'
#' @param tree STree object created by stree_split.
#' @param newdata Data frame of new observations.
#' @param return_probs Logical; if TRUE, returns class probabilities.
#'
#' @return Character vector of predictions or list with predictions and probabilities.
#'
#' @export
stree_predict <- function(tree, newdata, return_probs = FALSE) {

  # Batch prediction
  if (is.data.frame(newdata) && nrow(newdata) > 1) {
    preds <- lapply(seq_len(nrow(newdata)), function(i) {
      stree_predict(tree, newdata[i, , drop = FALSE], return_probs)
    })

    if (return_probs) {
      pred_classes <- sapply(preds, function(x) x$prediction)
      prob_matrix <- do.call(rbind, lapply(preds, function(x) x$probabilities))
      return(list(predictions = pred_classes, probabilities = prob_matrix))
    } else {
      return(unlist(preds))
    }
  }

  # Single observation or leaf node
  if (tree$is_leaf) {
    if (return_probs) {
      prob_vector <- tree$class_prob
      names(prob_vector) <- tree$class_names
      return(list(
        prediction = tree$prediction,
        probabilities = prob_vector
      ))
    } else {
      return(tree$prediction)
    }
  }

  # Get the features that were actually used in training
  newdata_subset <- newdata[, tree$features, drop = FALSE]

  # Convert factors to numeric (same as training)
  for (col in names(newdata_subset)) {
    if (is.factor(newdata_subset[[col]])) {
      newdata_subset[[col]] <- as.numeric(newdata_subset[[col]])
    } else if (is.character(newdata_subset[[col]])) {
      newdata_subset[[col]] <- as.numeric(as.factor(newdata_subset[[col]]))
    }
  }

  # Apply the same scaling used during training
  if (!is.null(tree$scaling_params)) {
    newdata_subset <- standard_scaler(newdata_subset, params = tree$scaling_params)
  }

  # Internal node: compute decision value
  dec_value <- attr(
    predict(tree$model, newdata_subset, decision.values = TRUE),
    "decision.values"
  )

  if (is.matrix(dec_value)) {
    dec_value <- dec_value[1, 1]
  } else {
    dec_value <- as.numeric(dec_value)[1]
  }

  # Traverse tree based on distance to hyperplane
  if (dec_value >= 0) {
    return(stree_predict(tree$left, newdata, return_probs))
  } else {
    return(stree_predict(tree$right, newdata, return_probs))
  }
}
