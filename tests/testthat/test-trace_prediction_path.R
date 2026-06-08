test_that("runs without error and produces non-empty console output", {
  tree   <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  output <- expect_no_error(
    capture.output(trace_prediction_path(tree, binary_data, sample_idx = 1))
  )
  expect_true(length(output) > 0)
})

test_that("returns a single character string (the predicted class)", {
  tree   <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- invisible(
    trace_prediction_path(tree, binary_data, sample_idx = 1)
  )
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("returned prediction is from the known class set", {
  tree   <- svm_split(binary_data, "label", max_depth = 2, min_samples = 5)
  result <- invisible(
    trace_prediction_path(tree, binary_data, sample_idx = 1)
  )
  expect_true(result %in% c("A", "B", "UNKNOWN"))
})
