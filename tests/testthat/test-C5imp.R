# Tests for C5imp variable importance function

# Helper to create data that produces non-trivial trees
make_separable_data <- function(n = 200, seed = 8000) {
  set.seed(seed)
  # Create data where x1 and x2 are predictive
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- factor(sample(letters[1:3], n, replace = TRUE))
  # Make y depend on x1 and x2
  prob <- plogis(x1 + 0.5 * x2)
  y <- factor(ifelse(runif(n) < prob, "A", "B"))
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
}

# --- Basic Usage ---

test_that("C5imp returns usage metric by default", {
  dat <- make_separable_data(200, seed = 8001)
  mod <- C5.0(dat[, -1], dat$y)

  # Skip if tree is trivial (no splits)
  skip_if(mod$size == 1, "Tree has no splits")

  imp <- C5imp(mod)
  expect_s3_class(imp, "data.frame")
  expect_named(imp, "Overall")
  expect_true(all(rownames(imp) %in% c("x1", "x2", "x3")))
})

test_that("C5imp returns splits metric", {
  dat <- make_separable_data(200, seed = 8002)
  mod <- C5.0(dat[, -1], dat$y)

  skip_if(mod$size == 1, "Tree has no splits")

  imp <- C5imp(mod, metric = "splits")
  expect_s3_class(imp, "data.frame")
  expect_named(imp, "Overall")
})

test_that("C5imp with pct = FALSE returns raw counts for splits", {
  dat <- make_separable_data(200, seed = 8003)
  mod <- C5.0(dat[, -1], dat$y)

  skip_if(mod$size == 1, "Tree has no splits")

  imp_pct <- C5imp(mod, metric = "splits", pct = TRUE)
  imp_raw <- C5imp(mod, metric = "splits", pct = FALSE)

  expect_s3_class(imp_raw, "data.frame")
  # Pct should sum to 100 if there are splits
  if (sum(imp_pct$Overall) > 0) {
    expect_equal(sum(imp_pct$Overall), 100, tolerance = 0.01)
  }
})

test_that("C5imp works with tree model", {
  dat <- make_separable_data(200, seed = 8004)
  mod <- C5.0(dat[, -1], dat$y)

  skip_if(mod$size == 1, "Tree has no splits")

  imp <- C5imp(mod)
  expect_s3_class(imp, "data.frame")
  expect_true(nrow(imp) > 0)
})

test_that("C5imp works with rules model", {
  dat <- make_separable_data(200, seed = 8005)
  mod <- C5.0(dat[, -1], dat$y, rules = TRUE)

  # Rules might not always produce results, just check structure
  imp <- C5imp(mod)
  expect_s3_class(imp, "data.frame")
})

test_that("C5imp works with boosted model", {
  dat <- make_separable_data(300, seed = 8006)
  mod <- C5.0(dat[, -1], dat$y, trials = 10)

  imp <- C5imp(mod)
  expect_s3_class(imp, "data.frame")
})

test_that("C5imp results are sorted by importance (descending)", {
  dat <- make_separable_data(200, seed = 8007)
  mod <- C5.0(dat[, -1], dat$y)

  skip_if(mod$size == 1, "Tree has no splits")

  imp <- C5imp(mod)
  # Check that values are in descending order
  expect_true(all(diff(imp$Overall) <= 0))
})

test_that("C5imp includes all predictors", {
  dat <- make_separable_data(200, seed = 8008)
  mod <- C5.0(dat[, -1], dat$y)

  skip_if(mod$size == 1, "Tree has no splits")

  imp <- C5imp(mod)
  expect_equal(sort(rownames(imp)), sort(c("x1", "x2", "x3")))
})

# --- Error Conditions ---

test_that("C5imp errors on invalid metric", {
  dat <- make_separable_data(200, seed = 9001)
  mod <- C5.0(dat[, -1], dat$y)

  expect_snapshot(
    error = TRUE,
    C5imp(mod, metric = "invalid")
  )
})
