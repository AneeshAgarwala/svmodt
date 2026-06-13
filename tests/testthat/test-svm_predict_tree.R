test_that("returns a character vector of the correct length", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  preds <- svm_predict_tree(tree, binary_data)

  expect_type(preds, "character")
  expect_length(preds, nrow(binary_data))
})

test_that("all predictions belong to the known class set", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  preds <- svm_predict_tree(tree, binary_data)

  expect_true(all(preds %in% c("A", "B")))
})

test_that("return_probs = TRUE yields a named list with 'predictions' and 'probabilities'", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data, return_probs = TRUE)

  expect_type(result, "list")
  expect_named(result, c("predictions", "probabilities"))
})

test_that("probability matrix has the correct number of rows and at least 2 columns", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data, return_probs = TRUE)

  expect_equal(nrow(result$probabilities), nrow(binary_data))
  expect_gte(ncol(result$probabilities), 2)
})

test_that("every row of the probability matrix sums to 1", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data, return_probs = TRUE)
  row_sums <- rowSums(result$probabilities)

  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("predicting on a leaf node returns the majority class for every row", {
  leaf <- leaf_node(factor(c("A", "A", "B")), 3, c("A", "B"))
  preds <- svm_predict_tree(leaf, binary_data[1:5, ])

  expect_length(preds, 5)
  expect_true(all(preds == "A"))
})

test_that("zero-row newdata is handled gracefully", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data[0, ], return_probs = TRUE)

  expect_length(result$predictions, 0)
  expect_equal(nrow(result$probabilities), 0)
})

test_that("achieves > 80% accuracy on clearly separable training data", {
  tree <- svm_split(binary_data, "label", max_depth = 3, min_samples = 3)
  preds <- svm_predict_tree(tree, binary_data)
  acc <- mean(preds == as.character(binary_data$label))

  expect_gt(acc, 0.80)
})

test_that("multiclass predictions are all within the known class set", {
  tree <- svm_split(multiclass_data, "label", max_depth = 3, min_samples = 5)
  preds <- svm_predict_tree(tree, multiclass_data)

  expect_true(all(preds %in% c("A", "B", "C")))
})


test_that("returns a numeric vector of the same length as input", {
  probs <- convert_decision_to_probs(c(-2, -1, 0, 1, 2))

  expect_type(probs, "double")
  expect_length(probs, 5)
})

test_that("all output values are strictly between 0 and 1", {
  probs <- convert_decision_to_probs(c(-10, 0, 10))

  expect_true(all(probs > 0))
  expect_true(all(probs < 1))
})

test_that("output is clipped to [0.001, 0.999]", {
  probs <- convert_decision_to_probs(c(-1e6, 1e6))

  expect_gte(min(probs), 0.001)
  expect_lte(max(probs), 0.999)
})

test_that("positive decision values produce probabilities > 0.5", {
  probs <- convert_decision_to_probs(c(1, 2, 5))
  expect_true(all(probs > 0.5))
})

test_that("negative decision values produce probabilities < 0.5", {
  probs <- convert_decision_to_probs(c(-1, -2, -5))
  expect_true(all(probs < 0.5))
})

test_that("returns an empty numeric vector for empty input", {
  result <- convert_decision_to_probs(numeric(0))
  expect_length(result, 0)
  expect_type(result, "double")
})

test_that("using model calibration does not error and stays within [0.001, 0.999]", {
  probs <- convert_decision_to_probs(c(-1, 0, 1), model = m$model)

  expect_gte(min(probs), 0.001)
  expect_lte(max(probs), 0.999)
})

# <U+2500><U+2500> calibrate_probs = FALSE <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("calibrate_probs = FALSE returns predictions for every row", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  preds <- svm_predict_tree(tree, binary_data, calibrate_probs = FALSE)

  expect_length(preds, nrow(binary_data))
  expect_true(all(preds %in% c("A", "B")))
})

test_that("calibrate_probs = FALSE with return_probs gives rows summing to 1", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data,
    return_probs    = TRUE,
    calibrate_probs = FALSE
  )

  row_sums <- rowSums(result$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})

# <U+2500><U+2500> multiclass probability shape <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("multiclass tree returns probability matrix with one column per class", {
  tree <- svm_split(multiclass_data, "label", max_depth = 3, min_samples = 5)
  result <- svm_predict_tree(tree, multiclass_data, return_probs = TRUE)

  expect_gte(ncol(result$probabilities), 3)
  expect_true(all(c("A", "B", "C") %in% colnames(result$probabilities)))
})

test_that("multiclass probability rows each sum to 1", {
  tree <- svm_split(multiclass_data, "label", max_depth = 3, min_samples = 5)
  result <- svm_predict_tree(tree, multiclass_data, return_probs = TRUE)
  row_sums <- rowSums(result$probabilities)

  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("multiclass predictions all belong to the known label set", {
  tree <- svm_split(multiclass_data, "label", max_depth = 3, min_samples = 5)
  preds <- svm_predict_tree(tree, multiclass_data)

  expect_true(all(preds %in% c("A", "B", "C")))
})

# <U+2500><U+2500> leaf-only tree (max_depth = 0) <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("depth-0 tree (single leaf) predicts the majority class for every sample", {
  # With max_depth = 0 the root is immediately a leaf
  tree <- svm_split(binary_data, "label", max_depth = 0)
  preds <- svm_predict_tree(tree, binary_data)

  # All predictions must be the same (the leaf's majority class)
  expect_length(unique(preds), 1)
  expect_true(unique(preds) %in% c("A", "B"))
})

# <U+2500><U+2500> probability matrix column naming <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("probability matrix columns are named with class labels", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- svm_predict_tree(tree, binary_data, return_probs = TRUE)

  expect_false(is.null(colnames(result$probabilities)))
  expect_true(all(colnames(result$probabilities) %in% c("A", "B")))
})

# <U+2500><U+2500> single-row newdata <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("works correctly with a single-row newdata frame", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  preds <- svm_predict_tree(tree, binary_data[1, , drop = FALSE])

  expect_length(preds, 1)
  expect_true(preds %in% c("A", "B"))
})

# <U+2500><U+2500> predictions on imbalanced data <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("tree with balanced weights still returns valid predictions", {
  tree <- svm_split(imbalanced_data, "label",
    max_depth     = 2,
    min_samples   = 5,
    class_weights = "balanced"
  )
  preds <- svm_predict_tree(tree, imbalanced_data)

  expect_length(preds, nrow(imbalanced_data))
  expect_true(all(preds %in% c("majority", "minority")))
})

# <U+2500><U+2500> get_fallback_predictions <U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500><U+2500>

test_that("returns a list with 'predictions' and 'probabilities'", {
  result <- get_fallback_predictions(
    model           = m$model,
    X_scaled        = m$scaler$train[1:5, ],
    decision_values = c(-1, -0.5, 0, 0.5, 1),
    svm_probs       = NULL,
    all_classes     = c("A", "B"),
    calibrate       = TRUE
  )

  expect_named(result, c("predictions", "probabilities"))
})

test_that("predictions vector has one element per sample", {
  n <- 8
  result <- get_fallback_predictions(
    model           = m$model,
    X_scaled        = m$scaler$train[1:n, ],
    decision_values = seq(-2, 2, length.out = n),
    svm_probs       = NULL,
    all_classes     = c("A", "B"),
    calibrate       = TRUE
  )

  expect_length(result$predictions, n)
})

test_that("probability matrix rows sum to 1", {
  n <- 6
  result <- get_fallback_predictions(
    model           = m$model,
    X_scaled        = m$scaler$train[1:n, ],
    decision_values = seq(-1, 1, length.out = n),
    svm_probs       = NULL,
    all_classes     = c("A", "B"),
    calibrate       = TRUE
  )

  row_sums <- rowSums(result$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("all predictions belong to the supplied all_classes", {
  result <- get_fallback_predictions(
    model           = m$model,
    X_scaled        = m$scaler$train[1:5, ],
    decision_values = c(-2, -1, 0, 1, 2),
    svm_probs       = NULL,
    all_classes     = c("A", "B"),
    calibrate       = TRUE
  )

  expect_true(all(result$predictions %in% c("A", "B")))
})

test_that("zero-sample input is handled gracefully", {
  result <- get_fallback_predictions(
    model           = m$model,
    X_scaled        = m$scaler$train[0, ],
    decision_values = numeric(0),
    svm_probs       = NULL,
    all_classes     = c("A", "B"),
    calibrate       = TRUE
  )

  expect_length(result$predictions, 0)
  expect_equal(nrow(result$probabilities), 0)
})

test_that("uses svm_probs when they are provided (Option 1 path)", {
  n <- 4
  # Craft a fake probability matrix that unambiguously favours class "A"
  fake_probs <- matrix(c(0.9, 0.1, 0.9, 0.1, 0.1, 0.9, 0.1, 0.9),
    nrow = n, ncol = 2
  )
  colnames(fake_probs) <- c("A", "B")

  result <- get_fallback_predictions(
    model           = m$model,
    X_scaled        = m$scaler$train[1:n, ],
    decision_values = rep(0, n),
    svm_probs       = fake_probs,
    all_classes     = c("A", "B"),
    calibrate       = TRUE
  )

  # First and third rows <U+2192> should predict "A" (prob 0.9)
  expect_equal(result$predictions[1], "A")
  expect_equal(result$predictions[3], "A")
  # Second and fourth rows <U+2192> should predict "B" (prob 0.9)
  expect_equal(result$predictions[2], "B")
  expect_equal(result$predictions[4], "B")
})

test_that("calibrate = FALSE falls back to training-class proportions", {
  result <- get_fallback_predictions(
    model           = m$model,
    X_scaled        = m$scaler$train[1:5, ],
    decision_values = c(-1, 0, 1, 0, -1),
    svm_probs       = NULL,
    all_classes     = c("A", "B"),
    calibrate       = FALSE
  )

  # All rows receive the same majority-class prediction (Option 3)
  expect_true(length(unique(result$predictions)) == 1)
  row_sums <- rowSums(result$probabilities)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})
