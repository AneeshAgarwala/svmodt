#' Calculate class weights for a node
#'
#' Computes class weights for a given set of target values based on the chosen
#' weighting strategy. Supports unweighted, balanced, balanced subsample, and
#' custom weighting schemes, with optional verbosity for diagnostic output.
#'
#' @param y A vector of class labels at the current node.
#' @param class_weights Character string specifying the weighting strategy.
#'   Options are:
#'   \itemize{
#'     \item `"none"` – no weighting (default).
#'     \item `"balanced"` – weights inversely proportional to class frequencies.
#'     \item `"balanced_subsample"` – weights adjusted to balance each subsample.
#'     \item `"custom"` – user-provided custom weights.
#'   }
#' @param custom_class_weights Named numeric vector of custom class weights
#'   (used only if `class_weights = "custom"`). Names must match the unique
#'   class labels in `y`.
#' @param verbose Logical; if `TRUE`, prints detailed information about
#'   computed weights.
#'
#' @return A named numeric vector of class weights for each unique class in `y`,
#'   or `NULL` if equal weights are used (`class_weights = "none"`) or if the
#'   custom weights are invalid.
#'
#' @details
#' The function caps computed class weights at 10 to avoid excessively large
#' scaling factors.
#'
#' @examples
#' \dontrun{
#' y <- c("A", "A", "B", "B", "B", "C")
#' calculate_node_class_weights(y, class_weights = "balanced", verbose = TRUE)
#' }
#'
#' @keywords internal
calculate_node_class_weights <- function(y, class_weights = "none",
                                         custom_class_weights = NULL,
                                         verbose = FALSE) {
  class_weights <- match.arg(class_weights, c("none", "balanced", "balanced_subsample", "custom"))

  # Get class distribution at current node
  class_counts <- table(y)
  class_names <- names(class_counts)

  if (class_weights == "none") {
    if (verbose) cat("Class weights: none (equal weights)\n")
    return(NULL)
  }

  if (class_weights == "custom") {
    if (is.null(custom_class_weights)) {
      warning("class_weights='custom' but no custom_class_weights provided. Using 'none' instead.")
      return(NULL)
    }

    # Validate custom weights
    if (!all(class_names %in% names(custom_class_weights))) {
      warning("Custom weights missing some classes. Using 'none' instead.")
      return(NULL)
    }

    weights <- custom_class_weights[class_names]
    weights <- pmin(weights, 10)

    if (verbose) {
      cat("Class weights (custom):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }
    return(weights)
  }

  if (class_weights == "balanced") {
    n_samples <- length(y)
    n_classes <- length(class_counts)
    weights <- n_samples / (n_classes * as.numeric(class_counts))
    names(weights) <- class_names

    weights <- pmin(weights, 10)

    if (verbose) {
      cat("Class weights (balanced):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }
    return(weights)
  }

  if (class_weights == "balanced_subsample") {
    weights <- 1 / as.numeric(class_counts)
    names(weights) <- class_names
    weights <- weights * (length(class_counts) / sum(weights))

    weights <- pmin(weights, 10)

    if (verbose) {
      cat("Class weights (balanced_subsample):", paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
    }
    return(weights)
  }
}
