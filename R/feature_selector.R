## Different Choices/Consideration for Feature Selection


## Gini Impurity for Variable Selection
gini <- function(labels) {
  counts <- tabulate(factor(labels))
  p <- counts / sum(counts)
  1 - sum(p * p)
}


## Entropy for Information Gain
entropy <- function(labels) {
  p <- prop.table(table(labels))
  -sum(p[p > 0] * log2(p[p > 0]))
}


## Information Gain Criteria for Variable Selection
info_gain <- function(feature, target) {
  total_entropy <- entropy(target)
  feature_levels <- unique(feature)

  weighted_entropy <- sum(
    sapply(feature_levels, function(level) {
      subset <- target[feature == level]
      (length(subset) / length(target)) * entropy(subset)
    })
  )

  ig <- total_entropy - weighted_entropy
  return(ig)
}


## Gain Ratio - Treats bias of Info Gain towards features with diverse set of values
gain_ratio <- function(target, feature) {
  ig <- info_gain(feature = feature, target = target)
  si <- entropy(feature) ## Intrinsic Info of the Feature calculated through Entropy
  if (si == 0) return(0)
  ig / si
}



## Wrapper Function for Feature Selection Criteria
feature_selector <- function(target, features, criteria_type = c("gini", "info_gain", "gain_ratio")) {

  criteria_type <- match.arg(criteria_type)

  best_feature <- NULL
  best_split <- NULL
  best_score <- -Inf

  for (fname in names(features)) {
    f <- features[[fname]]

    if (is.numeric(f)) {
      # Numeric split points
      unique_vals <- sort(unique(f))
      if (length(unique_vals) > 1) {
        split_points <- (unique_vals[-1] + unique_vals[-length(unique_vals)]) / 2
        for (sp in split_points) {
          split_factor <- factor(ifelse(f <= sp, "left", "right"))

          if (criteria_type == "gini") {
            left <- f <= sp
            right <- f > sp

            gini_left  <- gini(target[left])
            gini_right <- gini(target[right])

            weighted_gini <- (length(target[left]) / length(target)) * gini_left +
              (length(target[right]) / length(target)) * gini_right

            score <- -weighted_gini  # minimize impurity via negation

          } else if (criteria_type == "info_gain") {
            score <- info_gain(split_factor, target)

          } else if (criteria_type == "gain_ratio") {
            score <- gain_ratio(target, split_factor)
          }

          if (score > best_score) {
            best_score <- score
            best_feature <- fname
            best_split <- sp
          }
        }
      }

    } else {
      # Categorical feature
      lvls <- levels(factor(f))
      if (length(lvls) == 1) next

      for (lvl in lvls) {
        split_factor <- factor(ifelse(f == lvl, "left", "right"))

        if (criteria_type == "gini") {
          left <- f == lvl
          right <- f != lvl

          gini_left  <- gini(target[left])
          gini_right <- gini(target[right])

          weighted_gini <- (length(target[left]) / length(target)) * gini_left +
            (length(target[right]) / length(target)) * gini_right

          score <- -weighted_gini

        } else if (criteria_type == "info_gain") {
          score <- info_gain(split_factor, target)

        } else if (criteria_type == "gain_ratio") {
          score <- gain_ratio(target, split_factor)
        }

        if (score > best_score) {
          best_score <- score
          best_feature <- fname
          best_split <- lvl
        }
      }
    }
  }

  list(
    feature = best_feature,
    split = best_split,
    score = best_score
  )
}
