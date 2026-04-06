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
#     \item `"balanced_subsample"` – weights adjusted to balance each subsample.
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
#' @keywords internal
calculate_node_class_weights <- function(y, class_weights = "none",
                                         custom_class_weights = NULL,
                                         verbose = FALSE) {
  class_weights <- match.arg(
    class_weights,
    c(
      "none", "balanced",
      # "balanced_subsample",
      "custom"
    )
  )

  # ── Coerce y to character to avoid factor level type mismatches ─────────────
  # e1071::svm expects weight names to exactly match levels of y as seen
  # during fit — integer factor levels produce names like "1" but svm
  # internally sees factor level "1" vs integer 1 differently
  y_char <- as.character(y)
  class_table <- table(y_char)
  class_names <- names(class_table) # always character now

  if (verbose) {
    cat("Classes at node      :", paste(class_names, collapse = ", "), "\n")
    cat("Class counts         :", paste(class_table, collapse = ", "), "\n")
  }

  if (class_weights == "none") {
    if (verbose) cat("Class weights: none (equal weights)\n")
    return(NULL)
  }

  if (class_weights == "custom") {
    if (is.null(custom_class_weights)) {
      warning("class_weights = 'custom' but no custom_class_weights provided. Using 'none'.")
      return(NULL)
    }

    # Coerce custom weight names to character for consistent matching
    names(custom_class_weights) <- as.character(names(custom_class_weights))

    missing_classes <- setdiff(class_names, names(custom_class_weights))
    if (length(missing_classes) > 0) {
      warning(
        "Custom weights missing classes: ",
        paste(missing_classes, collapse = ", "), ". Using 'none'."
      )
      return(NULL)
    }

    weights <- custom_class_weights[class_names]
    weights <- pmin(weights, 10)
    names(weights) <- class_names

    if (verbose) {
      cat(
        "Class weights (custom):",
        paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n"
      )
    }
    return(weights)
  }

  if (class_weights == "balanced") {
    n_samples <- length(y_char)
    n_classes <- length(class_table)
    weights <- n_samples / (n_classes * as.numeric(class_table))
    weights <- pmin(weights, 10)
    names(weights) <- class_names

    if (verbose) {
      cat(
        "Class weights (balanced):",
        paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n"
      )
    }
    return(weights)
  }

  # if (class_weights == "balanced_subsample") {
  #   weights   <- 1 / as.numeric(class_table)
  #   weights   <- weights * (length(class_table) / sum(weights))
  #   weights   <- pmin(weights, 10)
  #   names(weights) <- class_names
  #
  #   if (verbose) {
  #     cat("Class weights (balanced_subsample):",
  #         paste(names(weights), "=", round(weights, 3), collapse = ", "), "\n")
  #   }
  #   return(weights)
  # }
}
