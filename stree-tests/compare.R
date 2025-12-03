library(reticulate)
library(dplyr)
library(tibble)

#' Diagnose why Python and R STree differ
#'
#' This function performs step-by-step comparison to identify divergence points
diagnose_stree_divergence <- function(data, response, py_model, r_model,
                                      max_depth = 3, verbose = TRUE) {

  cat("\n", strrep("=", 80), "\n")
  cat("STREE DIVERGENCE DIAGNOSTIC\n")
  cat(strrep("=", 80), "\n\n")

  # ============================================================
  # 1. DATA CHECKS
  # ============================================================
  cat("1. DATA VALIDATION\n")
  cat(strrep("-", 80), "\n")

  # Check sample counts
  n_samples <- nrow(data)
  cat("R data samples:", n_samples, "\n")

  # Check for NA values
  na_count <- sum(is.na(data))
  cat("NA values in R data:", na_count, "\n")

  # Check class distribution
  cat("\nClass distribution in R:\n")
  print(table(data[[response]]))

  # ============================================================
  # 2. SCALING COMPARISON
  # ============================================================
  cat("\n2. SCALING COMPARISON\n")
  cat(strrep("-", 80), "\n")

  X <- data[, names(data) != response]

  # R scaling (current method)
  X_r_scaled <- scale(X)

  # Python-compatible scaling (population std)
  X_py_scaled <- apply(X, 2, function(col) {
    m <- mean(col, na.rm = TRUE)
    s <- sqrt(sum((col - m)^2) / length(col))  # Population std
    if (s == 0) return(col - m)
    (col - m) / s
  })

  # Compare scaling
  scaling_diff <- max(abs(X_r_scaled - X_py_scaled), na.rm = TRUE)
  cat("Max scaling difference (R vs Python-style):",
      round(scaling_diff, 6), "\n")

  if (scaling_diff > 0.01) {
    cat("WARNING: Significant scaling differences detected!\n")
    cat("This will cause different SVM hyperplanes.\n\n")

    # Show columns with largest differences
    col_diffs <- apply(X_r_scaled - X_py_scaled, 2, function(x) max(abs(x), na.rm = TRUE))
    worst_cols <- head(sort(col_diffs, decreasing = TRUE), 5)
    cat("Top 5 features with scaling differences:\n")
    print(worst_cols)
  }

  # ============================================================
  # 3. ROOT NODE COMPARISON
  # ============================================================
  cat("\n3. ROOT NODE ANALYSIS\n")
  cat(strrep("-", 80), "\n")

  # Try all one-vs-rest splits manually
  y <- factor(data[[response]])
  present_classes <- levels(y)
  k <- length(present_classes)

  cat("Number of classes:", k, "\n")
  cat("Classes:", paste(present_classes, collapse = ", "), "\n\n")

  # Calculate impurity for each OVR split
  impurities <- list()

  for (target_class in present_classes) {
    y_binary <- ifelse(y == target_class, "positive", "negative")

    # Fit SVM with R scaling
    X_scaled <- scale(X)

    # Remove constant columns
    col_vars <- apply(X_scaled, 2, var, na.rm = TRUE)
    non_constant <- col_vars > 1e-10
    X_filt <- X_scaled[, non_constant, drop = FALSE]

    if (ncol(X_filt) == 0) {
      impurities[[target_class]] <- list(
        impurity = NA,
        error = "All features constant"
      )
      next
    }

    # Fit SVM
    svm_result <- tryCatch({
      model <- e1071::svm(
        x = X_filt,
        y = factor(y_binary),
        kernel = "linear",
        scale = FALSE,
        decision.values = TRUE
      )

      # Get decision values
      dec <- attr(predict(model, X_filt, decision.values = TRUE), "decision.values")
      decision_values <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)

      left_idx <- which(decision_values >= 0)
      right_idx <- which(decision_values < 0)

      if (length(left_idx) == 0 || length(right_idx) == 0) {
        list(impurity = NA, error = "Empty partition")
      } else {
        # Calculate weighted impurity on ORIGINAL labels
        y_left <- y[left_idx]
        y_right <- y[right_idx]

        impurity_left <- entropy(y_left)
        impurity_right <- entropy(y_right)

        weighted_impurity <- (length(left_idx) / length(y)) * impurity_left +
          (length(right_idx) / length(y)) * impurity_right

        list(
          impurity = weighted_impurity,
          n_left = length(left_idx),
          n_right = length(right_idx),
          left_dist = table(y_left),
          right_dist = table(y_right)
        )
      }
    }, error = function(e) {
      list(impurity = NA, error = e$message)
    })

    impurities[[target_class]] <- svm_result
  }

  # Show impurities
  cat("One-vs-Rest impurities computed by R:\n")
  for (cls in names(impurities)) {
    result <- impurities[[cls]]
    if (!is.na(result$impurity)) {
      cat(sprintf("  Class %s vs rest: %.4f (n_left=%d, n_right=%d)\n",
                  cls, result$impurity, result$n_left, result$n_right))
    } else {
      cat(sprintf("  Class %s vs rest: FAILED (%s)\n", cls, result$error))
    }
  }

  # Find best split
  valid_impurities <- sapply(impurities, function(x)
    if (!is.na(x$impurity)) x$impurity else Inf)
  best_class_r <- names(which.min(valid_impurities))
  best_impurity_r <- min(valid_impurities[is.finite(valid_impurities)])

  cat("\nR would choose:", best_class_r, "vs rest\n")
  cat("R best impurity:", round(best_impurity_r, 4), "\n")

  # Compare with Python output
  cat("\nFrom Python GraphViz output:\n")
  cat("Python root node has these characteristics to compare...\n")
  cat("(Check the GraphViz output to see what Python chose)\n")

  # ============================================================
  # 4. FEATURE SUBSET COMPARISON
  # ============================================================
  cat("\n4. FEATURE USAGE\n")
  cat(strrep("-", 80), "\n")

  # Extract features used at root
  py_root_features <- if (py_has_attr(py_model$tree_, "colsAtNode")) {
    py_to_r(py_model$tree_$colsAtNode)
  } else {
    NULL
  }

  r_root_features <- r_model$features

  cat("Python root features:",
      if (!is.null(py_root_features)) paste(py_root_features, collapse = ", ") else "ALL",
      "\n")
  cat("R root features:",
      if (!is.null(r_root_features)) paste(r_root_features, collapse = ", ") else "ALL",
      "\n")

  if (!is.null(py_root_features) && !is.null(r_root_features)) {
    features_match <- setequal(py_root_features, r_root_features)
    cat("Features match:", features_match, "\n")
  }

  # ============================================================
  # 5. HYPERPLANE COMPARISON (if available)
  # ============================================================
  cat("\n5. HYPERPLANE COMPARISON\n")
  cat(strrep("-", 80), "\n")

  # Python hyperplane
  if (py_has_attr(py_model$tree_, "clf")) {
    py_clf <- py_model$tree_$clf
    if (py_has_attr(py_clf, "coef_")) {
      py_coef <- py_to_r(py_clf$coef_)
      cat("Python hyperplane coefficients (first 5):\n")
      print(head(py_coef, 5))

      if (py_has_attr(py_clf, "intercept_")) {
        py_intercept <- py_to_r(py_clf$intercept_)
        cat("Python intercept:", py_intercept, "\n")
      }
    }
  }

  # R hyperplane
  if (!is.null(r_model$model)) {
    r_clf <- r_model$model
    if (!is.null(r_clf$coefs) && !is.null(r_clf$SV)) {
      r_coef <- as.vector(t(r_clf$coefs) %*% as.matrix(r_clf$SV))
      cat("\nR hyperplane coefficients (first 5):\n")
      print(head(r_coef, 5))

      if (!is.null(r_clf$rho)) {
        r_intercept <- -r_clf$rho
        cat("R intercept:", r_intercept, "\n")
      }
    }
  }

  # ============================================================
  # 6. RECOMMENDATIONS
  # ============================================================
  cat("\n6. RECOMMENDATIONS\n")
  cat(strrep("-", 80), "\n")

  issues <- c()

  if (scaling_diff > 0.01) {
    issues <- c(issues, "- Use custom standard_scaler() to match Python exactly")
  }

  if (na_count > 0) {
    issues <- c(issues, "- Handle NA values before training")
  }

  # Check for tie-breaking issues
  valid_imp_values <- valid_impurities[is.finite(valid_impurities)]
  if (length(valid_imp_values) > 1) {
    imp_range <- max(valid_imp_values) - min(valid_imp_values)
    if (imp_range < 0.01) {
      issues <- c(issues, "- Multiple splits have very similar impurities (tie-breaking differs)")
    }
  }

  if (length(issues) > 0) {
    cat("Identified issues:\n")
    for (issue in issues) {
      cat(issue, "\n")
    }
  } else {
    cat("No obvious issues found. Differences may be due to:\n")
    cat("- Random initialization in SVM solver\n")
    cat("- Numerical precision differences\n")
    cat("- Different SVM solvers (libsvm vs sklearn's implementation)\n")
  }

  return(invisible(list(
    impurities = impurities,
    best_class_r = best_class_r,
    scaling_diff = scaling_diff,
    issues = issues
  )))
}

#' Helper function to calculate entropy
entropy <- function(y) {
  if (length(y) == 0) return(0)
  probs <- table(y) / length(y)
  probs <- probs[probs > 0]
  -sum(probs * log2(probs))
}

#' Quick check: Are the predictions different?
check_prediction_agreement <- function(py_model, r_model, data, response) {
  # Python predictions
  X_mat <- data[, names(data) != response] |> as.matrix()
  py_preds <- py_model$predict(X_mat)

  # R predictions
  r_preds <- stree_predict(r_model, data)

  agreement <- mean(py_preds == r_preds)

  cat("\nPrediction Agreement:", round(agreement * 100, 2), "%\n")
  cat("Mismatches:", sum(py_preds != r_preds), "out of", length(py_preds), "\n")

  if (agreement < 1.0) {
    cat("\nSample mismatches:\n")
    mismatch_idx <- which(py_preds != r_preds)[1:min(10, sum(py_preds != r_preds))]
    mismatch_df <- data.frame(
      Index = mismatch_idx,
      True = data[[response]][mismatch_idx],
      Python = py_preds[mismatch_idx],
      R = r_preds[mismatch_idx]
    )
    print(mismatch_df)
  }

  return(agreement)
}

#Example usage:
diagnosis <- diagnose_stree_divergence(
  data = ctg10,
  response = "CLASS",
  py_model = svc_model,
  r_model = r_stree_model,
  verbose = TRUE
)
