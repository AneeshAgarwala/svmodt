test_that("constant strategy always returns base_max_features regardless of depth", {
  res <- calculate_dynamic_max_features(
    binary_data, "label", base_max_features = 2,
    depth = 5, strategy = "constant"
  )
  expect_equal(res, 2)
})

test_that("constant strategy with NULL base_max_features uses total feature count", {
  # binary_data has two predictors: x1, x2
  res <- calculate_dynamic_max_features(
    binary_data, "label", base_max_features = NULL,
    depth = 1, strategy = "constant"
  )
  expect_equal(res, 2)
})

test_that("decrease strategy returns fewer features at greater depth", {
  depth_1 <- calculate_dynamic_max_features(
    binary_data, "label", base_max_features = 2,
    depth = 1, strategy = "decrease", decrease_rate = 0.5
  )
  depth_3 <- calculate_dynamic_max_features(
    binary_data, "label", base_max_features = 2,
    depth = 3, strategy = "decrease", decrease_rate = 0.5
  )
  expect_gte(depth_1, depth_3)
})

test_that("decrease strategy never returns less than 1", {
  res <- calculate_dynamic_max_features(
    binary_data, "label", base_max_features = 2,
    depth = 100, strategy = "decrease", decrease_rate = 0.1
  )
  expect_gte(res, 1)
})

test_that("random strategy returns a value within the requested proportion range", {
  set.seed(1)
  res <- calculate_dynamic_max_features(
    binary_data, "label", base_max_features = NULL,
    depth = 1, strategy = "random",
    random_range = c(0.5, 1.0)
  )
  total <- 2L  # two predictors in binary_data
  expect_gte(res, max(2L, round(total * 0.5)))
  expect_lte(res, total)
})

test_that("result never exceeds the total number of available predictors", {
  # base_max_features deliberately set higher than the actual predictor count
  res <- calculate_dynamic_max_features(
    binary_data, "label", base_max_features = 100,
    depth = 1, strategy = "constant"
  )
  expect_lte(res, 2)
})

test_that("invalid decrease_rate is silently corrected and does not error", {
  expect_no_error(
    calculate_dynamic_max_features(
      binary_data, "label", base_max_features = 2,
      depth = 2, strategy = "decrease", decrease_rate = -0.5
    )
  )
})
