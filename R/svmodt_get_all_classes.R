#' Retrieve all class labels from a decision tree
#'
#' Recursively extracts all unique class labels stored in a decision tree’s
#' leaf nodes.
#'
#' @param tree A decision tree object, where each node may contain:
#'   \itemize{
#'     \item \code{is_leaf} – logical; \code{TRUE} if the node is a leaf.
#'     \item \code{class_prob} – named numeric vector of class probabilities.
#'     \item \code{left}, \code{right} – child node objects.
#'   }
#'
#' @return A character vector of all unique class labels present in the tree.
#'
#' @keywords internal
get_all_classes <- function(tree) {
  if (tree$is_leaf) {
    return(names(tree$class_prob))
  }
  classes <- character(0)
  if (!is.null(tree$left)) {
    classes <- c(classes, get_all_classes(tree$left))
  }
  if (!is.null(tree$right)) {
    classes <- c(classes, get_all_classes(tree$right))
  }
  return(unique(classes))
}
