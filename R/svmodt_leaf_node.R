#' @title Create a leaf node for a decision tree
#' @description
#' Constructs a leaf node object containing class probabilities, predicted class,
#' and metadata.
#'
#' @param y Vector of class labels for the samples in the node.
#' @param n Number of samples in the node.
#' @param all_classes Optional character vector of all possible classes. If NULL,
#'   classes are inferred from \code{y}.
#' @param features Character vector of features used at this node (default empty).
#' @param scaler Optional scaler object applied to the features at this node.
#'
#' @return A list representing a leaf node with components:
#' \itemize{
#'   \item \code{is_leaf} Logical; \code{TRUE}.
#'   \item \code{prediction} Predicted class (majority class in the node).
#'   \item \code{n} Number of samples in the node.
#'   \item \code{features} Features used at this node.
#'   \item \code{scaler} Optional scaler applied to the node features.
#'   \item \code{class_prob} Named numeric vector of class probabilities (sums to 1).
#' }
#'
#' @details
#' - If some classes are missing in \code{y}, probabilities for those classes are set to 0.
#' - If all probabilities are 0 or NA, a uniform probability distribution is used.
#' - Probabilities are normalized to sum to 1.
#'
#' @keywords internal
leaf_node <- function(y, n, all_classes = NULL, features = character(0), scaler = NULL) {
  # FIXED: Handle missing all_classes
  if (is.null(all_classes)) {
    all_classes <- unique(as.character(y))
  }

  # Compute class proportions
  class_table <- table(y)
  class_props <- prop.table(class_table)

  # Initialize probability vector for all classes
  prob_vec <- setNames(rep(0, length(all_classes)), all_classes)
  prob_vec[names(class_props)] <- as.numeric(class_props)

  # FIXED: Better safety check
  if (sum(prob_vec) == 0 || any(is.na(prob_vec)) || all(prob_vec == 0)) {
    prob_vec <- rep(1 / length(all_classes), length(all_classes))
    names(prob_vec) <- all_classes
  }

  # Normalize to ensure sum = 1
  prob_vec <- prob_vec / sum(prob_vec)

  list(
    is_leaf    = TRUE,
    prediction = names(which.max(prob_vec)),
    n          = n,
    features   = features,
    scaler     = scaler,
    class_prob = prob_vec
  )
}
