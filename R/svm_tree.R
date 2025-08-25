library(kernlab)

svm_split <- function(data, response, depth = 1, max_depth = 3, min_samples = 20, global_levels = NULL, debug = FALSE) {

  # Capture global levels at root
  if (is.null(global_levels)) {
    global_levels <- sort(unique(data[[response]]))
  }

  # Force response to be factor with global levels
  data[[response]] <- factor(data[[response]], levels = global_levels)

  # Stop conditions
  y_table <- table(data[[response]])
  if (depth > max_depth ||
      nrow(data) < min_samples ||
      length(unique(data[[response]])) == 1 ||
      min(y_table) < 2) {  # prevents rare class causing ksvm error
    return(list(
      is_leaf = TRUE,
      prediction = names(which.max(y_table)),
      n = nrow(data),
      class_dist = prop.table(y_table)
    ))
  }

  # Compute class proportions and better weights
  class_props <- y_table / sum(y_table)
  # Use inverse frequency weighting
  class_weights <- sum(y_table) / (length(y_table) * y_table)
  class_weights <- setNames(class_weights[global_levels], global_levels)
  class_weights[is.na(class_weights)] <- 1e-6

  # Optional debug
  if (debug) {
    cat("\n---- Node at depth", depth, "----\n")
    print(list(
      levels_in_data = levels(data[[response]]),
      global_levels = global_levels,
      class_props = class_props,
      class_weights = class_weights
    ))
  }

  # Fit SVM
  formula <- as.formula(paste(response, "~ ."))
  model <- ksvm(
    formula,
    data = data,
    kernel = "vanilladot",
    class.weights = class_weights,
    prob.model = TRUE
  )

  # Get decision values
  decision_vals <- predict(model, data, type = "decision")
  if (is.matrix(decision_vals)) decision_vals <- decision_vals[,1]

  # Split data
  left_idx  <- which(decision_vals < 0)
  right_idx <- which(decision_vals >= 0)

  # If split fails, make leaf
  if (length(left_idx) == 0 || length(right_idx) == 0) {
    return(list(
      is_leaf = TRUE,
      prediction = names(which.max(y_table)),
      n = nrow(data),
      class_dist = prop.table(y_table)
    ))
  }

  # Recursive calls
  left_child  <- svm_split(data[left_idx, ], response, depth + 1, max_depth, min_samples, global_levels, debug)
  right_child <- svm_split(data[right_idx, ], response, depth + 1, max_depth, min_samples, global_levels, debug)

  return(list(
    is_leaf = FALSE,
    model = model,
    left = left_child,
    right = right_child,
    depth = depth,
    n = nrow(data),
    class_dist = prop.table(y_table)
  ))
}

