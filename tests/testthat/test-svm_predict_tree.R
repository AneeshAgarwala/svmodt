test_that("returns a character vector of the correct length", {
  tree  <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  preds <- svm_predict_tree(tree, binary_data)

  expect_type(preds, "character")
  expect_length(preds, nrow(binary_data))
})

test_that("all predictions belong to the known class set", {
  tree  <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  preds <- svm_predict_tree(tree, binary_data)

  expect_true(all(preds %in% c("A", "B")))
})

test_that("return_probs = TRUE yields a named list with 'predictions' and 'probabilities'", {
  tree   <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data, return_probs = TRUE)

  expect_type(result, "list")
  expect_named(result, c("predictions", "probabilities"))
})

test_that("probability matrix has the correct number of rows and at least 2 columns", {
  tree   <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data, return_probs = TRUE)

  expect_equal(nrow(result$probabilities), nrow(binary_data))
  expect_gte(ncol(result$probabilities), 2)
})

test_that("every row of the probability matrix sums to 1", {
  tree     <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result   <- svm_predict_tree(tree, binary_data, return_probs = TRUE)
  row_sums <- rowSums(result$probabilities)

  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("predicting on a leaf node returns the majority class for every row", {
  leaf  <- leaf_node(factor(c("A", "A", "B")), 3, c("A", "B"))
  preds <- svm_predict_tree(leaf, binary_data[1:5, ])

  expect_length(preds, 5)
  expect_true(all(preds == "A"))
})

test_that("zero-row newdata is handled gracefully", {
  tree   <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data[0, ], return_probs = TRUE)

  expect_length(result$predictions, 0)
  expect_equal(nrow(result$probabilities), 0)
})

test_that("achieves > 80% accuracy on clearly separable training data", {
  tree  <- svm_split(binary_data, "label", max_depth = 3, min_samples = 3)
  preds <- svm_predict_tree(tree, binary_data)
  acc   <- mean(preds == as.character(binary_data$label))

  expect_gt(acc, 0.80)
})

test_that("multiclass predictions are all within the known class set", {
  tree  <- svm_split(multiclass_data, "label", max_depth = 3, min_samples = 5)
  preds <- svm_predict_tree(tree, multiclass_data)

  expect_true(all(preds %in% c("A", "B", "C")))
})
