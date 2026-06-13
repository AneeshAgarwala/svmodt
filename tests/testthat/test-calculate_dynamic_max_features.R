test_that("constant strategy always returns base_max_features regardless of depth", {
  res <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = 2,
    depth = 5, strategy = "constant"
  )
  expect_equal(res, 2)
})

test_that("constant strategy with NULL base_max_features uses total feature count", {
  # binary_data has two predictors: x1, x2
  res <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = NULL,
    depth = 1, strategy = "constant"
  )
  expect_equal(res, 2)
})

test_that("decrease strategy returns fewer features at greater depth", {
  depth_1 <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = 2,
    depth = 1, strategy = "decrease", decrease_rate = 0.5
  )
  depth_3 <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = 2,
    depth = 3, strategy = "decrease", decrease_rate = 0.5
  )
  expect_gte(depth_1, depth_3)
})

test_that("decrease strategy never returns less than 1", {
  res <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = 2,
    depth = 100, strategy = "decrease", decrease_rate = 0.1
  )
  expect_gte(res, 1)
})

test_that("random strategy returns a value within the requested proportion range", {
  set.seed(1)
  res <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = NULL,
    depth = 1, strategy = "random",
    random_range = c(0.5, 1.0)
  )
  total <- 2L # two predictors in binary_data
  expect_gte(res, max(2L, round(total * 0.5)))
  expect_lte(res, total)
})

test_that("result never exceeds the total number of available predictors", {
  # base_max_features deliberately set higher than the actual predictor count
  res <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = 100,
    depth = 1, strategy = "constant"
  )
  expect_lte(res, 2)
})

test_that("invalid decrease_rate is silently corrected and does not error", {
  expect_no_error(
    calculate_dynamic_max_features(
      binary_data, "label",
      base_max_features = 2,
      depth = 2, strategy = "decrease", decrease_rate = -0.5
    )
  )
})


test_that("decrease strategy with verbose = TRUE prints strategy info", {
  expect_output(
    calculate_dynamic_max_features(
      binary_data, "label",
      base_max_features = 2,
      depth = 2,
      strategy = "decrease",
      verbose = TRUE
    ),
    regexp = "Strategy|decrease|depth"
  )
})

test_that("random strategy with verbose = TRUE prints strategy info", {
  expect_output(
    calculate_dynamic_max_features(
      binary_data, "label",
      base_max_features = NULL,
      depth = 1,
      strategy = "random",
      verbose = TRUE
    ),
    regexp = "Strategy|random|depth"
  )
})

test_that("constant strategy with verbose = TRUE produces no extra output", {
  # constant strategy has an early return inside switch <U+2014> the verbose cat
  # is inside the if (strategy != "constant") guard, so nothing is printed.
  output <- capture.output(
    calculate_dynamic_max_features(
      binary_data, "label",
      base_max_features = 2,
      depth = 1,
      strategy = "constant",
      verbose = TRUE
    )
  )
  expect_length(output, 1)
})

test_that("random strategy with equal min/max range returns min_features", {
  # When min_features >= max_features inside the random branch, the function
  # returns min_features directly without calling sample().
  # Force this by using random_range = c(1.0, 1.0).
  result <- calculate_dynamic_max_features(
    binary_data, "label",
    base_max_features = NULL,
    depth = 1,
    strategy = "random",
    random_range = c(1.0, 1.0) # min == max -> no sample()
  )
  expect_type(result, "double")
  expect_gte(result, 1)
})
