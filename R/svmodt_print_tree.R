#' Print an SVM Decision Tree
#'
#' Recursively prints the structure of an SVM-based decision tree.
#'
#' @param tree The tree object to print.
#' @param indent String used for indentation (for recursive calls).
#' @param show_probabilities Logical; whether to display class probabilities at leaf nodes.
#' @param show_feature_info Logical; whether to show features used at nodes.
#' @param show_penalties Logical; whether to show penalty flags at nodes.
#' @return Invisibly returns NULL. Prints to console.
#' @examples
#' tree <- svm_split(
#'   data = wdbc,
#'   response = "diagnosis",
#'   max_features = 2,
#'   max_depth = 3,
#'   min_samples = 5,
#'   feature_method = "random",
#'   verbose = TRUE
#' )
#' print_svm_tree(tree)
#' @export
print_svm_tree <- function(tree, indent = "", show_probabilities = FALSE,
                               show_feature_info = TRUE, show_penalties = TRUE) {
  if (tree$is_leaf) {
    cat(indent, "[Leaf] predict =", tree$prediction, "| n =", tree$n)

    if (show_probabilities && !is.null(tree$class_prob)) {
      probs <- paste(names(tree$class_prob), "=", round(tree$class_prob, 3),
                     collapse = ", ")
      cat(" | probs = [", probs, "]", sep = "")
    }

    if (show_feature_info && length(tree$features) > 0) {
      cat(" | features = [", paste(tree$features, collapse = ","), "]", sep = "")
    }

    cat("\n")
    return(invisible())
  }

  cat(indent, "[Node] depth =", tree$depth, "| n =", tree$n)

  # Show OVR split information
  if (!is.null(tree$hyperplane_class)) {
    cat(" | split:", tree$hyperplane_class, "vs rest")
  }

  if (!is.null(tree$impurity)) {
    cat(" | impurity =", round(tree$impurity, 4))
  }

  if (show_feature_info) {
    cat(" | features = [", paste(tree$features, collapse = ","), "]", sep = "")
  }

  if (show_penalties && !is.null(tree$penalty_applied)) {
    penalty_symbol <- if (tree$penalty_applied) "!" else "+"
    cat(" | penalty =", penalty_symbol)
  }

  cat("\n")

  if (!is.null(tree$left) || !is.null(tree$right)) {
    cat(indent, "|- Positive branch (distance >= 0):\n")
    if (!is.null(tree$left)) {
      print_svm_tree(
        tree$left, paste0(indent, "|  "), show_probabilities,
        show_feature_info, show_penalties
      )
    } else {
      cat(indent, "|  (no left child)\n")
    }

    cat(indent, "`- Negative branch (distance < 0):\n")
    if (!is.null(tree$right)) {
      print_svm_tree(
        tree$right, paste0(indent, "   "), show_probabilities,
        show_feature_info, show_penalties
      )
    } else {
      cat(indent, "   (no right child)\n")
    }
  }

  invisible()
}
