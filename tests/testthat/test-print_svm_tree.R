test_that("produces console output containing 'Leaf' for a leaf node", {
  leaf <- leaf_node(factor(c("A", "B")), 2, c("A", "B"))
  expect_output(print_svm_tree(leaf), regexp = "Leaf")
})

test_that("produces console output for an internal node tree", {
  tree <- svm_split(binary_data, "label", max_depth = 1, min_samples = 5)
  expect_output(print_svm_tree(tree), regexp = "Node|Leaf")
})

test_that("returns NULL invisibly", {
  leaf   <- leaf_node(factor("A"), 1, c("A", "B"))
  result <- print_svm_tree(leaf)
  expect_null(result)
})

test_that("shows class probabilities when show_probabilities = TRUE", {
  leaf <- leaf_node(factor(c("A", "B")), 2, c("A", "B"))
  expect_output(
    print_svm_tree(leaf, show_probabilities = TRUE),
    regexp = "probs"
  )
})
