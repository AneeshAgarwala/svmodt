#' Trace Prediction Path for a Sample
#'
#' Shows the path taken by a single sample through the SVM tree,
#' including decision values, branches, and final prediction.
#'
#' @param tree The tree object.
#' @param sample_data Data frame containing the sample(s).
#' @param sample_idx Index of the sample to trace (default 1).
#' @return The predicted class for the sample. Prints path to console.
#' @examples
#' \dontrun{
#' trace_prediction_path(tree, test_data, sample_idx = 1)
#' }
#' @keywords internal
trace_prediction_path <- function(tree, sample_data, sample_idx = 1) {
  cat("=== Tracing Prediction Path ===\n")
  cat("Sample", sample_idx, ":\n")

  # Show the sample
  sample_row <- sample_data[sample_idx, , drop = FALSE]
  for (col in names(sample_row)) {
    cat("  ", col, "=", sample_row[[col]], "\n")
  }
  cat("\n")

  trace_path <- function(node, sample, path = character(0), depth = 1) {
    indent <- paste(rep("  ", depth - 1), collapse = "")

    if (node$is_leaf) {
      cat(
        indent, "[FINAL] Predict", node$prediction,
        "(n =", node$n, ")\n"
      )
      cat(indent, "Path taken:", paste(path, collapse = " -> "), "\n")
      return(node$prediction)
    }

    cat(
      indent, "[Node", depth, "] features =",
      paste(node$features, collapse = ","), "\n"
    )

    # Apply scaling and get decision
    X_scaled <- apply_scaler(sample[, node$features, drop = FALSE], node$scaler)
    dec <- attr(
      predict(node$model, X_scaled, decision.values = TRUE),
      "decision.values"
    )
    dec_val <- if (is.matrix(dec)) dec[1, 1] else as.numeric(dec)[1]

    cat(indent, "  SVM decision value:", round(dec_val, 4), "\n")

    if (dec_val > 0 && !is.null(node$left)) {
      cat(indent, "  -> Going LEFT (decision > 0)\n")
      return(trace_path(node$left, sample, c(path, "LEFT"), depth + 1))
    } else if (dec_val <= 0 && !is.null(node$right)) {
      cat(indent, "  -> Going RIGHT (decision <= 0)\n")
      return(trace_path(node$right, sample, c(path, "RIGHT"), depth + 1))
    } else {
      cat(indent, "  [WARNING] No valid child node - using fallback\n")
      # Fallback logic here
      return("UNKNOWN")
    }
  }

  prediction <- trace_path(tree, sample_row)
  cat("\nFinal prediction:", prediction, "\n")
  return(prediction)
}
