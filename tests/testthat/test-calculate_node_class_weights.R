test_that("returns NULL for 'none' strategy", {
  result <- calculate_node_class_weights(binary_data$label, class_weights = "none")
  expect_null(result)
})

test_that("returns a named numeric vector for 'balanced' strategy", {
  result <- calculate_node_class_weights(binary_data$label, class_weights = "balanced")

  expect_type(result, "double")
  expect_named(result)
  expect_true(all(result > 0))
})

test_that("balanced weights are inversely proportional to class frequencies", {
  result <- calculate_node_class_weights(imbalanced_data$label, class_weights = "balanced")

  # Minority class must receive a strictly higher weight than the majority class
  expect_gt(result["minority"], result["majority"])
})

test_that("balanced weights are capped at 10", {
  # Extreme imbalance forces the raw weight for 'B' well above 10
  y      <- factor(c(rep("A", 999), "B"))
  result <- calculate_node_class_weights(y, class_weights = "balanced")
  expect_true(all(result <= 10))
})

test_that("custom weights are returned exactly as provided", {
  cw     <- c(A = 2, B = 0.5)
  result <- calculate_node_class_weights(
    binary_data$label,
    class_weights        = "custom",
    custom_class_weights = cw
  )

  expect_equal(result["A"], cw["A"])
  expect_equal(result["B"], cw["B"])
})

test_that("custom strategy warns and returns NULL when custom_class_weights is NULL", {
  expect_warning(
    result <- calculate_node_class_weights(
      binary_data$label,
      class_weights        = "custom",
      custom_class_weights = NULL
    ),
    regexp = "no custom_class_weights"
  )
  expect_null(result)
})

test_that("custom strategy warns and returns NULL when a class is missing from weights", {
  cw <- c(A = 2)  # B is intentionally absent

  expect_warning(
    result <- calculate_node_class_weights(
      binary_data$label,
      class_weights        = "custom",
      custom_class_weights = cw
    ),
    regexp = "missing"
  )
  expect_null(result)
})

test_that("balanced strategy with verbose = TRUE prints weight info", {
  expect_output(
    calculate_node_class_weights(
      binary_data$label,
      class_weights = "balanced",
      verbose       = TRUE
    ),
    regexp = "balanced|weight"
  )
})

test_that("none strategy with verbose = TRUE prints 'equal weights'", {
  expect_output(
    calculate_node_class_weights(
      binary_data$label,
      class_weights = "none",
      verbose       = TRUE
    ),
    regexp = "equal|none"
  )
})

test_that("custom strategy with verbose = TRUE prints custom weight values", {
  expect_output(
    calculate_node_class_weights(
      binary_data$label,
      class_weights        = "custom",
      custom_class_weights = c(A = 1.5, B = 0.8),
      verbose              = TRUE
    ),
    regexp = "custom|weight"
  )
})

test_that("verbose = TRUE always prints class names and counts header", {
  expect_output(
    calculate_node_class_weights(
      binary_data$label,
      class_weights = "balanced",
      verbose       = TRUE
    ),
    regexp = "Classes at node|Class counts"
  )
})
