`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# FIXED: Better formatting and consistent parameter names
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

# Compact summary version
print_svm_tree_compact <- function(tree) {
  cat("=== SVM Tree Summary ===\n")
  stats <- tree_stats(tree)

  cat("Structure:", stats$n_nodes, "nodes,", stats$n_leaves, "leaves, max depth", stats$max_depth, "\n")
  cat("Memory:", round(stats$memory_usage_mb, 2), "MB\n")

  # Feature usage analysis
  if (exists("analyze_feature_usage")) {
    feature_analysis <- analyze_feature_usage(tree)
    total_features <- sum(feature_analysis$total_usage > 0)
    cat("Features: Using", total_features, "unique features, diversity =",
        round(feature_analysis$diversity_score, 3), "\n")
  }

  cat("\nTree Structure:\n")
  print_svm_tree(tree, show_probabilities = FALSE, show_feature_info = TRUE,
                 show_penalties = TRUE)
}

# Feature-focused printing
print_feature_usage <- function(tree, max_depth = NULL) {
  cat("=== Feature Usage by Depth ===\n")

  feature_by_depth <- list()
  penalty_by_depth <- list()

  collect_info <- function(node, depth = 1) {
    if (is.null(max_depth) || depth <= max_depth) {
      if (!node$is_leaf) {
        # Collect features used at this depth
        if (is.null(feature_by_depth[[depth]])) {
          feature_by_depth[[depth]] <<- character(0)
        }
        feature_by_depth[[depth]] <<- c(feature_by_depth[[depth]], node$features)

        # Collect penalty information
        if (!is.null(node$penalty_applied) && node$penalty_applied) {
          penalty_by_depth[[depth]] <<- c(penalty_by_depth[[depth]] %||% character(0),
                                          paste0("Node_", node$n))
        }

        # Recurse
        if (!is.null(node$left)) collect_info(node$left, depth + 1)
        if (!is.null(node$right)) collect_info(node$right, depth + 1)
      }
    }
  }

  collect_info(tree)

  for (depth in sort(as.numeric(names(feature_by_depth)))) {
    features <- feature_by_depth[[depth]]
    unique_features <- unique(features)
    feature_counts <- table(features)

    cat("Depth", depth, ":\n")
    cat("  Features used:", paste(unique_features, collapse = ", "), "\n")
    cat("  Usage counts:", paste(names(feature_counts), "(", feature_counts, ")",
                                 collapse = ", "), "\n")

    if (depth %in% names(penalty_by_depth)) {
      cat("  Penalties applied:", length(penalty_by_depth[[depth]]), "nodes\n")
    }

    cat("\n")
  }
}

# Comparative printing for multiple trees
print_tree_comparison <- function(..., names = NULL) {
  trees <- list(...)

  if (is.null(names)) {
    names <- paste("Tree", seq_along(trees))
  }

  cat("=== Tree Comparison ===\n")

  # Create comparison table
  comparison_data <- data.frame(
    Tree = names,
    Nodes = sapply(trees, function(t) tree_stats(t)$n_nodes),
    Leaves = sapply(trees, function(t) tree_stats(t)$n_leaves),
    Max_Depth = sapply(trees, function(t) tree_stats(t)$max_depth),
    Memory_MB = sapply(trees, function(t) round(tree_stats(t)$memory_usage_mb, 2)),
    stringsAsFactors = FALSE
  )

  # Add feature diversity if function exists
  if (exists("analyze_feature_usage")) {
    comparison_data$Feature_Diversity <- sapply(trees, function(t) {
      round(analyze_feature_usage(t)$diversity_score, 3)
    })
    comparison_data$Unique_Features <- sapply(trees, function(t) {
      analysis <- analyze_feature_usage(t)
      sum(analysis$total_usage > 0)
    })
  }

  print(comparison_data, row.names = FALSE)

  cat("\nDetailed Structure:\n")
  for (i in seq_along(trees)) {
    cat("\n", rep("=", 50), "\n", sep = "")
    cat(names[i], "\n")
    cat(rep("=", 50), "\n")
    print_svm_tree(trees[[i]], show_probabilities = FALSE,
                   show_feature_info = TRUE, show_penalties = TRUE)
  }
}

# Interactive tree explorer (for large trees)
explore_tree <- function(tree, max_depth = 3) {
  cat("=== Interactive Tree Explorer ===\n")
  cat("Showing tree up to depth", max_depth, "\n")
  cat("Use print_svm_tree(tree) to see full tree\n\n")

  print_limited_depth <- function(node, current_depth = 1, indent = "") {
    if (current_depth > max_depth) {
      cat(indent, "... (depth limit reached, ",
          count_nodes_below(node), " more nodes below)\n")
      return()
    }

    if (node$is_leaf) {
      cat(indent, "🍃 Leaf: predict =", node$prediction, "| n =", node$n, "\n")
      return()
    }

    cat(indent, "🌳 Node: depth =", node$depth, "| n =", node$n,
        "| features = [", paste(node$features, collapse = ","), "]\n")

    if (!is.null(node$left)) {
      cat(indent, "├─ Left:\n")
      print_limited_depth(node$left, current_depth + 1, paste0(indent, "│  "))
    }

    if (!is.null(node$right)) {
      cat(indent, "└─ Right:\n")
      print_limited_depth(node$right, current_depth + 1, paste0(indent, "   "))
    }
  }

  print_limited_depth(tree)
}

# Helper function to count nodes
count_nodes_below <- function(node) {
  if (node$is_leaf) return(1)

  count <- 1
  if (!is.null(node$left)) count <- count + count_nodes_below(node$left)
  if (!is.null(node$right)) count <- count + count_nodes_below(node$right)

  return(count)
}

# Path tracing for specific predictions
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
