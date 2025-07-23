## Print Method
print_tree <- function(node, depth = 0) {
  indent <- paste0(rep("  ", depth), collapse = "")

  if (!is.null(node$prediction)) {
    cat(indent, "Predict:", node$prediction, "\n")
  } else {
    cat(indent, node$split_feature, "<=", node$split_value, "\n")
    if (!is.null(node$left)) {
      cat(indent, "Left:\n")
      print_tree(node$left, depth + 1)
    }
    if (!is.null(node$right)) {
      cat(indent, "Right:\n")
      print_tree(node$right, depth + 1)
    }
  }
}
