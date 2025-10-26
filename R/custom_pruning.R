# Count leaves in a (sub)tree
count_leaves <- function(node) {
  if (is.null(node$split_feature)) {
    return(1)
  }
  count_leaves(node$left) + count_leaves(node$right)
}

# Subtree misclassification error (weighted by sample counts)
subtree_error <- function(node) {
  if (is.null(node$split_feature)) {
    return(1 - max(node$probs))
  }
  ln <- node$left$n
  rn <- node$right$n
  le <- subtree_error(node$left)
  re <- subtree_error(node$right)
  (ln * le + rn * re) / (ln + rn)
}
