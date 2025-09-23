leaf_node <- function(y, n, all_classes, features = character(0), scaler = NULL) {

  # Compute class proportions
  p <- prop.table(table(y))

  # Initialize probability vector for all classes
  prob_vec <- setNames(rep(0, length(all_classes)), all_classes)
  prob_vec[names(p)] <- as.numeric(p)

  # Safety: if all probabilities are zero (empty y), assign uniform distribution
  if (sum(prob_vec) == 0 || any(is.na(prob_vec))) {
    prob_vec <- rep(1/length(prob_vec), length(prob_vec))
  }

  # Normalize to ensure sum = 1
  prob_vec <- prob_vec / sum(prob_vec)

  list(
    is_leaf    = TRUE,
    prediction = names(which.max(prob_vec)),  # safest: pick max probability
    n          = n,
    features   = features,
    scaler     = scaler,
    class_prob = prob_vec
  )
}
