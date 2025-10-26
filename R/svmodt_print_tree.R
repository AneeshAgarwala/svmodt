# `%||%` <- function(a, b) {
#   if (is.null(a)) b else a
# }

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
#' \dontrun{
#' print_svm_tree(tree)
#' }
#' @export
print_svm_tree <- function(tree, indent = "", show_probabilities = FALSE,
                           show_feature_info = TRUE, show_penalties = TRUE) {

  if (tree$is_leaf) {
    cat(indent, "🍃 Leaf: predict =", tree$prediction, "| n =", tree$n)

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

  cat(indent, "🌳 Node: depth =", tree$depth, "| n =", tree$n)

  if (show_feature_info) {
    cat(" | features = [", paste(tree$features, collapse = ","), "]", sep = "")

    if (!is.null(tree$max_features_used)) {
      cat(" | max_feat =", tree$max_features_used)
    }
  }

  if (show_penalties && !is.null(tree$penalty_applied)) {
    penalty_symbol <- if (tree$penalty_applied) "⚠️" else "✓"
    cat(" | penalty =", penalty_symbol)
  }

  cat("\n")

  if (!is.null(tree$left) || !is.null(tree$right)) {
    cat(indent, "├─ Left branch (SVM > 0):\n")
    if (!is.null(tree$left)) {
      print_svm_tree(tree$left, paste0(indent, "│  "), show_probabilities,
                     show_feature_info, show_penalties)
    } else {
      cat(indent, "│  (no left child)\n")
    }

    cat(indent, "└─ Right branch (SVM ≤ 0):\n")
    if (!is.null(tree$right)) {
      print_svm_tree(tree$right, paste0(indent, "   "), show_probabilities,
                     show_feature_info, show_penalties)
    } else {
      cat(indent, "   (no right child)\n")
    }
  }

  invisible()
}


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
#' @export
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
      cat(indent, "🍃 FINAL: Predict", node$prediction,
          "(n =", node$n, ")\n")
      cat(indent, "Path taken:", paste(path, collapse = " → "), "\n")
      return(node$prediction)
    }

    cat(indent, "🌳 Node", depth, ": features =",
        paste(node$features, collapse = ","), "\n")

    # Apply scaling and get decision
    X_scaled <- apply_scaler(sample[, node$features, drop = FALSE], node$scaler)
    dec <- attr(predict(node$model, X_scaled, decision.values = TRUE),
                "decision.values")
    dec_val <- if (is.matrix(dec)) dec[1, 1] else as.numeric(dec)[1]

    cat(indent, "  SVM decision value:", round(dec_val, 4), "\n")

    if (dec_val > 0 && !is.null(node$left)) {
      cat(indent, "  → Going LEFT (decision > 0)\n")
      return(trace_path(node$left, sample, c(path, "LEFT"), depth + 1))
    } else if (dec_val <= 0 && !is.null(node$right)) {
      cat(indent, "  → Going RIGHT (decision ≤ 0)\n")
      return(trace_path(node$right, sample, c(path, "RIGHT"), depth + 1))
    } else {
      cat(indent, "  ⚠️  No valid child node - using fallback\n")
      # Fallback logic here
      return("UNKNOWN")
    }
  }

  prediction <- trace_path(tree, sample_row)
  cat("\nFinal prediction:", prediction, "\n")
  return(prediction)
}
