print_dtksvm_tree <- function(tree, indent = 0) {
  prefix <- paste(rep("  ", indent), collapse = "")

  if (tree$is_leaf) {
    cat(prefix, "Leaf Node: n =", tree$n,
        ", prediction =", tree$prediction,
        ", class_dist = {",
        paste(names(tree$class_dist), round(tree$class_dist, 3), sep = ":", collapse = ", "),
        "}\n")
  } else {
    cat(prefix, "Internal Node: n =", tree$n, ", depth =", tree$depth, "\n")

    # Optional: show dominant class at this node
    major_class <- names(which.max(tree$class_dist))
    cat(prefix, " Dominant class at node:", major_class, "\n")

    # Recurse left
    cat(prefix, " Left ->\n")
    print_svm_tree(tree$left, indent + 1)

    # Recurse right
    cat(prefix, " Right ->\n")
    print_svm_tree(tree$right, indent + 1)
  }
}
