print_svm_tree <- function(tree, indent = "") {
  if (tree$is_leaf) {
    cat(indent, "Leaf: predict =", tree$prediction,
        "| n =", tree$n, "\n")
    return()
  }
  cat(indent, "Node: depth =", tree$depth,
      "| n =", tree$n,
      "| features =", paste(tree$features, collapse = ","), "\n")

  cat(indent, "Left branch:\n")
  if (!is.null(tree$left)) {
    print_svm_tree(tree$left, paste0(indent, "  "))
  } else {
    cat(indent, "  (no left child)\n")
  }

  cat(indent, "Right branch:\n")
  if (!is.null(tree$right)) {
    print_svm_tree(tree$right, paste0(indent, "  "))
  } else {
    cat(indent, "  (no right child)\n")
  }
}
