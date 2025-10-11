leaf_node <- function(y, n, all_classes = NULL, features = character(0), scaler = NULL) {
  # FIXED: Handle missing all_classes
  if (is.null(all_classes)) {
    all_classes <- unique(as.character(y))
  }

  # Compute class proportions
  class_table <- table(y)
  class_props <- prop.table(class_table)

  # Initialize probability vector for all classes
  prob_vec <- setNames(rep(0, length(all_classes)), all_classes)
  prob_vec[names(class_props)] <- as.numeric(class_props)

  # FIXED: Better safety check
  if (sum(prob_vec) == 0 || any(is.na(prob_vec)) || all(prob_vec == 0)) {
    prob_vec <- rep(1/length(all_classes), length(all_classes))
    names(prob_vec) <- all_classes
  }

  # Normalize to ensure sum = 1
  prob_vec <- prob_vec / sum(prob_vec)

  list(
    is_leaf    = TRUE,
    prediction = names(which.max(prob_vec)),
    n          = n,
    features   = features,
    scaler     = scaler,
    class_prob = prob_vec
  )
}
