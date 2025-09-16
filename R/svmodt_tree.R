library(e1071)

library(e1071)

svm_split <- function(data, response, depth = 1, max_depth = 3,
                      min_samples = 5, max_features = NULL,
                      feature_method = c("random", "mutual", "cor"), ...) {

  cat("\n--- Node at depth", depth, "---\n")
  cat("Rows:", nrow(data), "\n")
  cat("Response distribution:\n")
  print(table(data[[response]]))

  if (anyNA(data)) {
    cat("Warning: NA values detected! Stopping here.\n")
    return(list(is_leaf = TRUE,
                prediction = names(which.max(table(data[[response]]))),
                n = nrow(data),
                features = character(0),
                scaler = NULL))
  }

  y <- data[[response]]

  # --- Stopping conditions ---
  if (depth > max_depth || length(unique(y)) == 1 || nrow(data) < min_samples) {
    reason <- ifelse(depth > max_depth, "max depth reached",
                     ifelse(length(unique(y)) == 1, "pure node", "too few samples"))
    cat("Stopping:", reason, "\n")
    return(list(is_leaf = TRUE,
                prediction = names(which.max(table(y))),
                n = nrow(data),
                features = character(0),
                scaler = NULL))
  }

  # --- Feature selection ---
  if (!is.null(max_features)) {
    features <- choose_features(data, response, max_features, method = feature_method)
  } else {
    features <- setdiff(names(data), response)
  }

  if (length(features) == 0) {
    cat("Stopping at depth", depth, "- no usable features\n")
    return(list(is_leaf = TRUE,
                prediction = names(which.max(table(y))),
                n = nrow(data),
                features = character(0),
                scaler = NULL))
  }

  cat("Using features:", paste(features, collapse = ", "), "\n")

  # --- Scaling ---
  scaler <- scale_node(data[features])  # returns list(train, transform)
  X_scaled <- scaler$train

  # --- Train SVM without storing full dataset ---
  model <- tryCatch(
    svm(x = X_scaled, y = y, kernel = "linear", decision.values = TRUE,
        probability = FALSE, scale = FALSE, ...),
    error = function(e) {
      cat("SVM failed:", e$message, "\n")
      return(NULL)
    }
  )

  if (is.null(model)) {
    return(list(is_leaf = TRUE,
                prediction = names(which.max(table(y))),
                n = nrow(data),
                features = features,
                scaler = scaler))
  }

  # --- Decision values ---
  dec <- attr(predict(model, X_scaled, decision.values = TRUE), "decision.values")
  dec_vals <- if (is.matrix(dec)) dec[, 1] else as.numeric(dec)

  left_idx  <- which(dec_vals > 0)
  right_idx <- which(dec_vals <= 0)
  cat("Left size:", length(left_idx), "Right size:", length(right_idx), "\n")

  # --- Handle small child nodes ---
  if (length(left_idx) < min_samples && length(right_idx) < min_samples) {
    cat("Stopping: both child nodes too small\n")
    return(list(is_leaf = TRUE,
                prediction = names(which.max(table(y))),
                n = nrow(data),
                features = features,
                scaler = scaler))
  }

  # --- Single-child nodes ---
  if (length(left_idx) < min_samples) {
    cat("Left child too small, only right child\n")
    right_child <- svm_split(data[right_idx, , drop = FALSE], response,
                             depth + 1, max_depth, min_samples,
                             max_features, feature_method, ...)
    return(list(
      is_leaf = FALSE,
      model = model,
      features = features,
      scaler = scaler,
      best_col = 1,
      left = NULL,
      right = right_child,
      depth = depth,
      n = nrow(data)
    ))
  }

  if (length(right_idx) < min_samples) {
    cat("Right child too small, only left child\n")
    left_child <- svm_split(data[left_idx, , drop = FALSE], response,
                            depth + 1, max_depth, min_samples,
                            max_features, feature_method, ...)
    return(list(
      is_leaf = FALSE,
      model = model,
      features = features,
      scaler = scaler,
      best_col = 1,
      left = left_child,
      right = NULL,
      depth = depth,
      n = nrow(data)
    ))
  }

  # --- Normal recursive splits ---
  left  <- svm_split(data[left_idx, , drop = FALSE], response,
                     depth + 1, max_depth, min_samples,
                     max_features, feature_method, ...)
  right <- svm_split(data[right_idx, , drop = FALSE], response,
                     depth + 1, max_depth, min_samples,
                     max_features, feature_method, ...)

  list(
    is_leaf = FALSE,
    model = model,
    features = features,
    scaler = scaler,
    best_col = 1,
    left = left,
    right = right,
    depth = depth,
    n = nrow(data)
  )
}
