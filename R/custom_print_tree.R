## Pretty-print decision tree (works with pruned tree structure)
print_tree <- function(node, depth = 0) {
  indent <- paste0(rep("  ", depth), collapse = "")

  if (is.null(node$split_feature)) {
    # Leaf node
    cat(indent, "Predict:", node$prediction,
        "(n =", node$n, ")\n")
  } else {
    # Internal node
    cat(indent, node$split_feature, "<=", node$split_value,
        "(n =", node$n, ")\n")

    cat(indent, " Left:\n")
    print_tree(node$left, depth + 1)

    cat(indent, " Right:\n")
    print_tree(node$right, depth + 1)
  }
}
