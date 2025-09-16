svm_split <- function(data, response, depth = 1, max_depth = 30, min_samples = 5,
                      global_levels = NULL, debug = FALSE, root_data = NULL,
                      purity_threshold = 0.95, predict_type = c("decision", "probabilities"),
                      indices = NULL) {

  predict_type <- match.arg(predict_type)

  # Track original indices
  if (is.null(indices)) indices <- seq_len(nrow(data))

  if (is.null(root_data)) root_data <- data
  if (is.null(global_levels)) global_levels <- sort(unique(data[[response]]))

  data[[response]] <- factor(data[[response]], levels = global_levels)
  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      data[[col]] <- factor(data[[col]], levels = levels(root_data[[col]]))
    }
  }

  y_table <- table(data[[response]])
  class_dist <- prop.table(y_table)
  major_class_prop <- max(class_dist)

  # Stopping condition
  if (depth > max_depth || nrow(data) < min_samples ||
      length(unique(data[[response]])) == 1 ||
      min(y_table) < 2 || major_class_prop >= purity_threshold) {
    return(list(
      is_leaf = TRUE,
      prediction = names(which.max(y_table)),
      n = nrow(data),
      class_dist = class_dist,
      indices = indices
    ))
  }

  # Class weights
  class_weights <- sum(y_table) / (length(y_table) * y_table)
  class_weights <- setNames(class_weights[global_levels], global_levels)
  class_weights[is.na(class_weights)] <- 1e-6

  if (debug) {
    cat("\n---- Node at depth", depth, "----\n")
    print(list(
      levels_in_data = levels(data[[response]]),
      global_levels = global_levels,
      class_dist = class_dist,
      major_class_prop = major_class_prop,
      class_weights = class_weights
    ))
  }

  formula <- as.formula(paste(response, "~ ."))
  tmp_model <- ksvm(
    formula,
    data = data,
    kernel = "vanilladot",
    class.weights = class_weights,
    prob.model = TRUE
  )

  # --- Determine splitting indices based on predict_type ---
  if (predict_type == "decision") {
    decision_vals <- predict(tmp_model, data, type = "decision")
    if (is.matrix(decision_vals)) decision_vals <- decision_vals[,1]
    left_idx  <- which(decision_vals < 0)
    right_idx <- which(decision_vals >= 0)
  } else if (predict_type == "probabilities") {
    probs <- predict(tmp_model, data, type = "probabilities")
    if (is.matrix(probs)) {
      max_prob_class <- apply(probs, 1, which.max)
      left_idx <- which(max_prob_class == 1)
      right_idx <- which(max_prob_class != 1)
    } else {
      left_idx <- which(probs < 0.5)
      right_idx <- which(probs >= 0.5)
    }
  }

  # Subset data and track original indices
  left_data  <- data[left_idx, , drop = FALSE]
  right_data <- data[right_idx, , drop = FALSE]
  left_indices <- indices[left_idx]
  right_indices <- indices[right_idx]

  # Stop if child nodes too small
  if (nrow(left_data) < min_samples || nrow(right_data) < min_samples ||
      length(unique(left_data[[response]])) < 2 ||
      length(unique(right_data[[response]])) < 2) {
    return(list(
      is_leaf = TRUE,
      prediction = names(which.max(y_table)),
      n = nrow(data),
      class_dist = class_dist,
      indices = indices
    ))
  }

  # Recursive calls
  left_child  <- svm_split(left_data, response, depth + 1,
                           max_depth, min_samples, global_levels, debug, root_data, purity_threshold,
                           predict_type, left_indices)
  right_child <- svm_split(right_data, response, depth + 1,
                           max_depth, min_samples, global_levels, debug, root_data, purity_threshold,
                           predict_type, right_indices)

  return(list(
    is_leaf = FALSE,
    model = tmp_model,
    left = left_child,
    right = right_child,
    depth = depth,
    n = nrow(data),
    class_dist = class_dist,
    indices = indices
  ))
}
