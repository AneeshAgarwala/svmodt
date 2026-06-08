test_that("info_gain is positive when the feature perfectly separates the classes", {
  feature <- factor(c("L", "L", "L", "R", "R", "R"))
  target  <- factor(c("A", "A", "A", "B", "B", "B"))

  expect_gt(info_gain(feature, target, metric = "entropy"), 0)
})

test_that("info_gain is non-negative for entropy and gini on random data", {
  set.seed(42)
  feature <- factor(sample(c("L", "R"), 20, replace = TRUE))
  target  <- factor(sample(c("A", "B", "C"), 20, replace = TRUE))

  expect_gte(info_gain(feature, target, metric = "entropy"), 0)
  expect_gte(info_gain(feature, target, metric = "gini"),    0)
})

test_that("info_gain with gini is also positive for a perfect split", {
  feature <- factor(c("L", "L", "R", "R"))
  target  <- factor(c("A", "A", "B", "B"))

  expect_gt(info_gain(feature, target, metric = "gini"), 0)
})
