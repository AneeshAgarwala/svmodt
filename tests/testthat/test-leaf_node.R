test_that("returns a list with all required fields", {
  node <- leaf_node(factor(c("A", "A", "B")), n = 3, all_classes = c("A", "B"))

  expect_true(node$is_leaf)
  expect_equal(node$n, 3)
  expect_type(node$class_prob, "double")
  expect_named(node$class_prob)
  expect_false(is.null(node$prediction))
})

test_that("prediction is the majority class", {
  node <- leaf_node(factor(c("A", "A", "A", "B")), n = 4, all_classes = c("A", "B"))
  expect_equal(node$prediction, "A")
})

test_that("class probabilities sum to 1", {
  node <- leaf_node(factor(c("A", "B", "B", "C")), n = 4, all_classes = c("A", "B", "C"))
  expect_equal(sum(node$class_prob), 1, tolerance = 1e-9)
})

test_that("classes absent from y receive probability 0", {
  node <- leaf_node(factor(c("A", "A", "A")), n = 3, all_classes = c("A", "B", "C"))
  expect_equal(unname(node$class_prob["B"]), 0)
  expect_equal(unname(node$class_prob["C"]), 0)
})

test_that("safety fallback gives a valid uniform distribution when all probs would be 0", {
  # y contains a class that is not in all_classes — every named entry stays 0
  node <- leaf_node(factor(c("D", "D")), n = 2, all_classes = c("A", "B"))

  expect_equal(sum(node$class_prob), 1, tolerance = 1e-9)
  expect_true(all(node$class_prob > 0))
})

test_that("NULL all_classes is inferred from y", {
  node <- leaf_node(factor(c("X", "X", "Y")), n = 3, all_classes = NULL)
  expect_setequal(names(node$class_prob), c("X", "Y"))
})
