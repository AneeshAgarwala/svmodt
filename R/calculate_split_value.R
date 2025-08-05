calculate_split_value <- function(target, split_vals, criteria_type){
  unique_vals <- sort(unique(split_vals))
  if (length(unique_vals) < 2) {
    probs <- class_probabilities(target)
    prediction <- names(probs)[which.max(probs)]
    return(list(prediction = prediction, probs = probs))
  }
  candidate_splits <- (head(unique_vals, -1) + tail(unique_vals, -1)) / 2

  # Evaluate each candidate split
  best_score <- if (criteria_type == "gini") Inf else -Inf
  best_split_value <- NA

  for (val in candidate_splits) {
    left_idx <- split_vals <= val
    right_idx <- !left_idx
    if (any(left_idx) && any(right_idx)) {
      score <- compute_split_score(target[left_idx], target[right_idx], criteria_type)
      if ((criteria_type == "gini" && score < best_score) ||
          (criteria_type != "gini" && score > best_score)) {
        best_score <- score
        best_split_value <- val
      }
    }
  }
  return(best_split_value)
}
