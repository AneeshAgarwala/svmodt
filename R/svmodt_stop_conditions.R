#' Check Stopping Conditions for Tree Splitting
#'
#' Internal utility function to determine if a node in a tree should stop
#' splitting based on depth, purity, or minimum sample size.
#'
#' @param data A data frame of predictor features at the current node.
#' @param y A vector of target values corresponding to \code{data}.
#' @param depth Current depth of the node in the tree.
#' @param max_depth Maximum allowed depth for the tree.
#' @param min_samples Minimum number of samples required to split a node.
#' @param verbose Logical; if \code{TRUE}, prints the reason for stopping.
#'
#' @return Logical; \code{TRUE} if the node meets any stopping condition, \code{FALSE} otherwise.
#'
#' @details
#' - Stops if the node reaches \code{max_depth}.
#' - Stops if all target values in the node are identical (\emph{pure node}).
#' - Stops if the number of samples is less than \code{min_samples}.
#'
#' @keywords internal
stop_conditions_met <- function(data, y, depth, max_depth, min_samples, verbose) {
  reason <- NULL
  if (depth > max_depth) {
    reason <- "max depth reached"
  } else if (length(unique(y)) == 1) {
    reason <- "pure node"
  } else if (nrow(data) < min_samples) {
    reason <- "too few samples"
  }
  if (!is.null(reason) && verbose) cat("Stopping:", reason, "\n")
  !is.null(reason)
}
