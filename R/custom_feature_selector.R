## Gini Impurity for Variable Selection
# Impurity measures
gini <- function(labels) {
  if (length(labels) == 0) {
    return(0)
  }
  p <- prop.table(table(labels))
  1 - sum(p * p)
}

entropy <- function(labels) {
  if (length(labels) == 0) {
    return(0)
  }
  p <- prop.table(table(labels))
  -sum(p[p > 0] * log2(p[p > 0]))
}

# Information gain on a factor split variable (2-level factor usually)
info_gain <- function(feature, target, metric = c("entropy", "gini")) {
  metric <- match.arg(metric)
  impurity_fun <- if (metric == "entropy") entropy else gini

  parent <- impurity_fun(target)

  lvl <- levels(factor(feature))
  weighted_child <- 0
  for (lv in lvl) {
    idx <- feature == lv
    weighted_child <- weighted_child + (sum(idx) / length(target)) * impurity_fun(target[idx])
  }
  parent - weighted_child
}

# Gain ratio (uses entropy for intrinsic info)
gain_ratio <- function(target, feature_factor) {
  ig <- info_gain(feature_factor, target, metric = "entropy")
  # Intrinsic information of the split variable
  p <- prop.table(table(feature_factor))
  si <- if (length(p)) -sum(p[p > 0] * log2(p[p > 0])) else 0
  if (si == 0) {
    return(0)
  }
  ig / si
}



# Evaluate a single binary split given a feature vector and split rule
.evaluate_split_score <- function(target, f, sp, criteria_type, ig_metric) {
  if (is.numeric(f)) {
    left <- f <= sp
  } else {
    left <- f == sp
  }
  right <- !left

  # skip invalid splits that create empty child
  if (sum(left) == 0 || sum(right) == 0) {
    return(-Inf)
  }

  split_factor <- factor(ifelse(left, "left", "right"), levels = c("left", "right"))

  if (criteria_type == "gini") {
    wl <- (sum(left) / length(target)) * gini(target[left])
    wr <- (sum(right) / length(target)) * gini(target[right])
    score <- -(wl + wr) # minimize impurity (negate)
  } else if (criteria_type == "info_gain") {
    score <- info_gain(split_factor, target, metric = ig_metric)
  } else { # gain_ratio
    score <- gain_ratio(target, split_factor)
  }

  score
}

# Choose best feature and split point/level
feature_selector <- function(target, features,
                             criteria_type = c("gini", "info_gain", "gain_ratio"),
                             ig_metric = c("gini", "entropy")) {
  criteria_type <- match.arg(criteria_type)
  if (missing(ig_metric)) ig_metric <- "gini"
  ig_metric <- match.arg(ig_metric)

  best_feature <- NULL
  best_split <- NULL
  best_score <- -Inf

  for (fname in names(features)) {
    f <- features[[fname]]

    if (is.numeric(f)) {
      # consider midpoints between unique sorted values where target label can change
      ord <- order(f)
      f_sorted <- f[ord]
      y_sorted <- target[ord]
      uniq <- which(diff(f_sorted) != 0)

      if (length(uniq) >= 1) {
        # consider split candidates only where the class changes across boundary
        change <- which(y_sorted[-length(y_sorted)] != y_sorted[-1])
        candidates_idx <- intersect(uniq, change)
        if (length(candidates_idx) == 0) candidates_idx <- uniq

        split_points <- (f_sorted[candidates_idx] + f_sorted[candidates_idx + 1]) / 2
        for (sp in split_points) {
          score <- .evaluate_split_score(target, f, sp, criteria_type, ig_metric)
          if (score > best_score) {
            best_score <- score
            best_feature <- fname
            best_split <- sp
          }
        }
      }
    } else {
      lvls <- levels(factor(f))
      if (length(lvls) <= 1) next
      for (lvl in lvls) {
        score <- .evaluate_split_score(target, f, lvl, criteria_type, ig_metric)
        if (score > best_score) {
          best_score <- score
          best_feature <- fname
          best_split <- lvl
        }
      }
    }
  }

  list(feature = best_feature, split = best_split, score = best_score)
}
