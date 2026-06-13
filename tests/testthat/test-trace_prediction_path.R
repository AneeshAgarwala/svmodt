test_that("runs without error and produces non-empty console output", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  output <- expect_no_error(
    capture.output(trace_prediction_path(tree, binary_data, sample_idx = 1))
  )
  expect_true(length(output) > 0)
})

test_that("returns a single character string (the predicted class)", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- invisible(
    trace_prediction_path(tree, binary_data, sample_idx = 1)
  )
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("returned prediction is from the known class set", {
  tree <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- invisible(
    trace_prediction_path(tree, binary_data, sample_idx = 1)
  )
  expect_true(result %in% c("A", "B", "UNKNOWN"))
})

test_that("trace_prediction_path returns 'UNKNOWN' when no child node exists", {
  scaler <- scale_node(binary_data[, c("x1", "x2")])
  node <- list(
    is_leaf = FALSE,
    model = fitted_model <- fit_svm_with_weights(
      scaler$train, binary_data$label,
      class_weights_vec = NULL
    ),
    features = c("x1", "x2"),
    scaler = scaler,
    best_col = 1,
    left = NULL,
    right = NULL,
    depth = 1,
    n = nrow(binary_data)
  )

  result <- suppressWarnings(
    capture.output(
      pred <- trace_prediction_path(node, binary_data, sample_idx = 1)
    )
  )
  expect_equal(pred, "UNKNOWN")
})
