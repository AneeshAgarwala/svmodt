# gini
test_that("gini returns 0 for a pure node", {
  expect_equal(gini(c("A", "A", "A", "A")), 0)
})

test_that("gini returns 0.5 for a perfectly balanced binary node", {
  expect_equal(gini(c("A", "A", "B", "B")), 0.5)
})

test_that("gini returns 0 for empty input", {
  expect_equal(gini(character(0)), 0)
})

test_that("gini is always between 0 and 1", {
  set.seed(1)
  y <- sample(c("A", "B", "C"), 30, replace = TRUE)
  expect_gte(gini(y), 0)
  expect_lte(gini(y), 1)
})

# entropy
test_that("entropy returns 0 for a pure node", {
  expect_equal(entropy(c("A", "A", "A")), 0)
})

test_that("entropy returns 0 for empty input", {
  expect_equal(entropy(character(0)), 0)
})

test_that("entropy is always non-negative", {
  set.seed(2)
  y <- sample(letters[1:4], 40, replace = TRUE)
  expect_gte(entropy(y), 0)
})

# calculate_impurity — consistency with gini / entropy
test_that("calculate_impurity with method='gini' matches gini()", {
  y <- c("A", "B", "B", "C")
  expect_equal(calculate_impurity(y, method = "gini"), gini(y), tolerance = 1e-12)
})

test_that("calculate_impurity with method='entropy' matches entropy()", {
  y <- c("A", "B", "B", "C")
  expect_equal(calculate_impurity(y, method = "entropy"), entropy(y), tolerance = 1e-12)
})
