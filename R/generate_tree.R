# Recursive function to generate a decision tree
generate_tree <- function(features, depth = 1, max_depth = 5, p_stop = 0.3) {
  # Random stopping condition - not mandatory included only to create unbalanced trees
  if (depth >= max_depth || runif(1) < p_stop) {
    return(list(prediction = paste("Class", sample(LETTERS, 1))))
  }

  # Randomly choose a feature - to be replaced with (Informartion Gain/Gain Ration/Gini Impurity) functions for feature selection
  split_feature <- sample(features, 1)
  # Randomly chose a split value - SVM for optimal value
  split_value <- round(runif(1, 0, 10), 2)  # Random split between 0 and 10

  # Recursively generate left and right subtrees
  left_subtree <- generate_tree(features, depth + 1, max_depth, p_stop)
  right_subtree <- generate_tree(features, depth + 1, max_depth, p_stop)

  # Return current node
  list(
    split_feature = split_feature,
    split_value = split_value,
    left = left_subtree,
    right = right_subtree
  )
}


