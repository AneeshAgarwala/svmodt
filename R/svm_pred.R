svm_tree_predict <- function(tree, newdata, return_probs = FALSE) {

  n <- nrow(newdata)

  # Initialize result
  if (return_probs) {
    # Prepare empty probability matrix with all classes
    all_classes <- names(tree$class_dist)
    preds <- matrix(0, nrow = n, ncol = length(all_classes))
    colnames(preds) <- all_classes
  } else {
    preds <- character(n)
  }

  # Recursive prediction helper
  predict_node <- function(node, data_idx) {
    if (length(data_idx) == 0) return()

    node_data <- newdata[data_idx, , drop = FALSE]

    if (node$is_leaf) {
      if (return_probs) {
        # Repeat leaf class distribution for all rows
        preds[data_idx, ] <<- matrix(rep(node$class_dist, each = length(data_idx)),
                                     nrow = length(data_idx))
      } else {
        preds[data_idx] <<- rep(node$prediction, length(data_idx))
      }
      return()
    }

    # Internal node: compute decision values
    decision_vals <- predict(node$model, node_data, type = "decision")
    if (is.matrix(decision_vals)) decision_vals <- decision_vals[,1]

    # Split indices
    left_idx  <- data_idx[which(decision_vals < 0)]
    right_idx <- data_idx[which(decision_vals >= 0)]

    # Recurse
    predict_node(node$left, left_idx)
    predict_node(node$right, right_idx)
  }

  # Start recursion from root
  predict_node(tree, 1:n)

  return(preds)
}
