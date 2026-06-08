test_that("returns TRUE when max_depth is exceeded", {
  expect_true(
    stop_conditions_met(
      binary_data, binary_data$label,
      depth = 11, max_depth = 10,
      min_samples = 2, verbose = FALSE
    )
  )
})

test_that("returns TRUE for a pure node (single class)", {
  pure_y <- factor(rep("A", 10))
  expect_true(
    stop_conditions_met(
      binary_data[1:10, ], pure_y,
      depth = 1, max_depth = 10,
      min_samples = 2, verbose = FALSE
    )
  )
})

test_that("returns TRUE when sample count is below min_samples", {
  small_df <- binary_data[1:3, ]
  expect_true(
    stop_conditions_met(
      small_df, small_df$label,
      depth = 1, max_depth = 10,
      min_samples = 5, verbose = FALSE
    )
  )
})

test_that("returns FALSE when no stopping condition is met", {
  expect_false(
    stop_conditions_met(
      binary_data, binary_data$label,
      depth = 1, max_depth = 10,
      min_samples = 2, verbose = FALSE
    )
  )
})

test_that("prints the stopping reason to the console when verbose = TRUE", {
  expect_output(
    stop_conditions_met(
      binary_data, binary_data$label,
      depth = 11, max_depth = 10,
      min_samples = 2, verbose = TRUE
    ),
    regexp = "max depth"
  )
})
