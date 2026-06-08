test_that("returns class names from a single leaf node", {
  leaf   <- leaf_node(factor(c("A", "A", "B")), n = 3, all_classes = c("A", "B", "C"))
  result <- get_all_classes(leaf)
  expect_setequal(result, c("A", "B", "C"))
})

test_that("aggregates classes from left and right child leaf nodes", {
  leaf_left  <- leaf_node(factor("A"), 1, all_classes = c("A", "B"))
  leaf_right <- leaf_node(factor("B"), 1, all_classes = c("A", "B"))

  internal <- list(is_leaf = FALSE, left = leaf_left, right = leaf_right)
  result   <- get_all_classes(internal)

  expect_setequal(result, c("A", "B"))
})

test_that("returns unique classes only — no duplicates", {
  leaf1    <- leaf_node(factor("A"), 1, all_classes = c("A", "B"))
  leaf2    <- leaf_node(factor("A"), 1, all_classes = c("A", "B"))
  internal <- list(is_leaf = FALSE, left = leaf1, right = leaf2)

  result <- get_all_classes(internal)

  # "A" must appear exactly once
  expect_length(result[result == "A"], 1)
})
