# Helper: Split score based on criterion
compute_split_score <- function(left_target, right_target, criterion) {
  n_total <- length(left_target) + length(right_target)
  w_left <- length(left_target) / n_total
  w_right <- length(right_target) / n_total

  if (criterion == "gini") {
    return(w_left * gini(left_target) + w_right * gini(right_target))
  } else if (criterion == "info_gain") {
    total_entropy <- entropy(c(left_target, right_target))
    weighted_entropy <- w_left * entropy(left_target) + w_right * entropy(right_target)
    return(total_entropy - weighted_entropy)
  } else if (criterion == "gain_ratio") {
    feature_vec <- c(rep("L", length(left_target)), rep("R", length(right_target)))
    ig <- compute_split_score(left_target, right_target, "info_gain")
    si <- intrinsic_info(feature_vec)
    return(if (si == 0) 0 else ig / si)
  } else {
    stop("Unknown criterion.")
  }
}
