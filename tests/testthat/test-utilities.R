# Tests for utility functions

# --- makeCostFile tests ---

test_that("makeCostFile creates cost string for costs > 1", {
  costs <- matrix(1, 2, 2)
  costs[1, 2] <- 2
  colnames(costs) <- c("A", "B")
  rownames(costs) <- c("A", "B")

  result <- C50:::makeCostFile(costs)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_true(grepl("A, B: 2", result))
})

test_that("makeCostFile returns empty string when all costs = 1", {
  costs <- matrix(1, 2, 2)
  colnames(costs) <- c("A", "B")
  rownames(costs) <- c("A", "B")

  result <- C50:::makeCostFile(costs)
  expect_equal(result, "")
})

test_that("makeCostFile handles asymmetric costs", {
  costs <- matrix(1, 3, 3)
  costs[1, 2] <- 2
  costs[2, 3] <- 3
  costs[3, 1] <- 4
  colnames(costs) <- c("A", "B", "C")
  rownames(costs) <- c("A", "B", "C")

  result <- C50:::makeCostFile(costs)
  expect_true(grepl("A, B: 2", result))
  expect_true(grepl("B, C: 3", result))
  expect_true(grepl("C, A: 4", result))
})

test_that("makeCostFile ignores diagonal elements", {
  costs <- matrix(1, 2, 2)
  costs[1, 1] <- 5 # Diagonal - should be ignored
  costs[2, 2] <- 5 # Diagonal - should be ignored
  colnames(costs) <- c("A", "B")
  rownames(costs) <- c("A", "B")

  result <- C50:::makeCostFile(costs)
  expect_equal(result, "")
})

# --- formatCharacters tests ---

test_that("formatCharacters handles normal text", {
  result <- C50:::formatCharacters("normal_text")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("formatCharacters converts colons to periods", {
  result <- C50:::formatCharacters("time:value")
  expect_true(grepl("\\.", result))
  expect_false(grepl(":", result))
})

test_that("formatCharacters escapes special characters", {
  # Comma should be escaped
  result <- C50:::formatCharacters("a,b")
  expect_true(grepl("\\\\,", result))
})

test_that("formatCharacters escapes periods", {
  result <- C50:::formatCharacters("a.b")
  expect_true(grepl("\\\\\\.", result))
})

test_that("formatCharacters escapes pipes", {
  result <- C50:::formatCharacters("a|b")
  expect_true(grepl("\\\\\\|", result))
})

test_that("formatCharacters handles multiple special characters", {
  result <- C50:::formatCharacters("a,b.c|d")
  expect_type(result, "character")
  # All special chars should be escaped
  expect_true(grepl("\\\\,", result))
  expect_true(grepl("\\\\\\.", result))
  expect_true(grepl("\\\\\\|", result))
})
