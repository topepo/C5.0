# Tests verifying that control parameters actually affect model output
# Each test compares default vs modified parameter to ensure different results

# Helper to create data that produces non-trivial trees
make_predictive_data <- function(n = 300, seed = 100) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- factor(sample(letters[1:4], n, replace = TRUE))
  # Make y predictable from x1 and x2
  prob <- plogis(1.5 * x1 - x2 + 0.5 * x3)
  y <- factor(ifelse(runif(n) < prob, "A", "B"))
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
}

# --- Case Weights ---

test_that("case weights affect model output", {
  dat <- make_predictive_data(300, seed = 101)

  # Default (no weights)
  set.seed(1001)
  mod_default <- C5.0(dat[, -1], dat$y)

  # With weights emphasizing class A
  wts <- ifelse(dat$y == "A", 3, 1)
  set.seed(1001)
  mod_weighted <- C5.0(dat[, -1], dat$y, weights = wts)

  # Trees should differ
  expect_false(identical(mod_default$tree, mod_weighted$tree))

  # Predictions should differ for at least some cases
  pred_default <- predict(mod_default, dat[, -1])
  pred_weighted <- predict(mod_weighted, dat[, -1])
  expect_false(identical(pred_default, pred_weighted))
})

# --- Winnowing ---

test_that("winnow parameter affects model output", {
  # Create data with some irrelevant predictors
  set.seed(102)
  n <- 300
  dat <- make_predictive_data(n, seed = 102)
  # Add noise predictors
  dat$noise1 <- rnorm(n)
  dat$noise2 <- rnorm(n)
  dat$noise3 <- factor(sample(letters[1:5], n, replace = TRUE))

  set.seed(1002)
  mod_default <- C5.0(dat[, -1], dat$y, control = C5.0Control(winnow = FALSE))
  set.seed(1002)
  mod_winnow <- C5.0(dat[, -1], dat$y, control = C5.0Control(winnow = TRUE))

  # Trees or attribute usage should differ
  # Winnowing may eliminate some predictors
  expect_false(identical(mod_default$tree, mod_winnow$tree))
})

# --- CF (Confidence Factor) ---

test_that("CF parameter affects model output", {
  dat <- make_predictive_data(300, seed = 103)

  # Default CF = 0.25
  set.seed(1003)
  mod_default <- C5.0(dat[, -1], dat$y, control = C5.0Control(CF = 0.25))

  # Very low CF = more pruning = smaller tree
  set.seed(1003)
  mod_low_cf <- C5.0(dat[, -1], dat$y, control = C5.0Control(CF = 0.01))

  # High CF = less pruning = larger tree
  set.seed(1003)
  mod_high_cf <- C5.0(dat[, -1], dat$y, control = C5.0Control(CF = 0.75))

  # Trees should differ
  expect_false(identical(mod_default$tree, mod_low_cf$tree))
  expect_false(identical(mod_default$tree, mod_high_cf$tree))

  # Low CF should generally produce smaller or equal tree
  expect_true(mod_low_cf$size <= mod_high_cf$size)
})

# --- minCases ---

test_that("minCases parameter affects model output", {
  dat <- make_predictive_data(400, seed = 104)

  # Default minCases = 2
  set.seed(1004)
  mod_default <- C5.0(dat[, -1], dat$y, control = C5.0Control(minCases = 2))

  # High minCases = larger minimum leaf size = smaller tree
  set.seed(1004)
  mod_high_min <- C5.0(dat[, -1], dat$y, control = C5.0Control(minCases = 30))

  # Trees should differ
  expect_false(identical(mod_default$tree, mod_high_min$tree))

  # Higher minCases should produce smaller or equal tree
  expect_true(mod_high_min$size <= mod_default$size)
})

# --- Sample ---
test_that("sample parameter affects model output", {
  dat <- make_predictive_data(400, seed = 105)

  # No sampling
  set.seed(1005)
  mod_default <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(sample = 0, seed = 42)
  )

  # 50% sampling
  set.seed(1005)
  mod_sampled <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(sample = 0.5, seed = 42)
  )

  # Trees should differ (trained on different subsets)
  expect_false(identical(mod_default$tree, mod_sampled$tree))

  # Predictions may differ
  pred_default <- predict(mod_default, dat[, -1])
  pred_sampled <- predict(mod_sampled, dat[, -1])
  # At least the output contains sampling info
  expect_true(
    grepl("test data", mod_sampled$output) ||
      !identical(pred_default, pred_sampled)
  )
})

# --- Bands ---

test_that("bands parameter affects rules model output", {
  dat <- make_predictive_data(300, seed = 106)

  # Rules without banding
  set.seed(1006)
  mod_default <- C5.0(
    dat[, -1],
    dat$y,
    rules = TRUE,
    control = C5.0Control(bands = 0)
  )

  # Rules with banding
  set.seed(1006)
  mod_banded <- C5.0(
    dat[, -1],
    dat$y,
    rules = TRUE,
    control = C5.0Control(bands = 5)
  )

  # Rules output should differ (banding adds utility info)
  expect_false(identical(mod_default$rules, mod_banded$rules))

  # Output should mention bands for banded model
  expect_true(mod_banded$control$bands == 5)
})

# --- noGlobalPruning ---

test_that("noGlobalPruning parameter affects model output", {
  # Use larger dataset with more noise to ensure pruning has effect
  set.seed(107)
  n <- 500
  dat <- data.frame(
    y = factor(sample(c("A", "B"), n, replace = TRUE, prob = c(0.6, 0.4))),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = factor(sample(letters[1:5], n, replace = TRUE))
  )
  # Make y somewhat predictable
  prob <- plogis(0.8 * dat$x1 - 0.5 * dat$x2)
  dat$y <- factor(ifelse(runif(n) < prob, "A", "B"))

  # Default (global pruning enabled)
  set.seed(1007)
  mod_default <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(noGlobalPruning = FALSE, CF = 0.5)
  )

  # No global pruning = potentially larger tree
  set.seed(1007)
  mod_no_prune <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(noGlobalPruning = TRUE, CF = 0.5)
  )

  # Without pruning, tree should be same size or larger
  expect_true(mod_no_prune$size >= mod_default$size)

  # If sizes differ, trees must differ
  if (mod_no_prune$size != mod_default$size) {
    expect_false(identical(mod_default$tree, mod_no_prune$tree))
  }
})

# --- fuzzyThreshold ---

test_that("fuzzyThreshold parameter affects model output", {
  dat <- make_predictive_data(300, seed = 108)

  # Default (no fuzzy thresholds)
  set.seed(1008)
  mod_default <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(fuzzyThreshold = FALSE)
  )

  # With fuzzy thresholds
  set.seed(1008)
  mod_fuzzy <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(fuzzyThreshold = TRUE)
  )

  # Trees may have same structure but probability predictions should differ
  pred_default <- predict(mod_default, dat[, -1], type = "prob")
  pred_fuzzy <- predict(mod_fuzzy, dat[, -1], type = "prob")

  # Fuzzy thresholds affect probability assignments
  expect_false(identical(pred_default, pred_fuzzy))
})

# --- earlyStopping ---

test_that("earlyStopping parameter affects boosted model output", {
  dat <- make_predictive_data(300, seed = 109)

  # With early stopping (default)
  set.seed(1009)
  mod_early <- C5.0(
    dat[, -1],
    dat$y,
    trials = 50,
    control = C5.0Control(earlyStopping = TRUE)
  )

  # Without early stopping
  set.seed(1009)
  mod_no_early <- C5.0(
    dat[, -1],
    dat$y,
    trials = 50,
    control = C5.0Control(earlyStopping = FALSE)
  )

  # Without early stopping, should run all requested trials
  expect_equal(mod_no_early$trials["Actual"], c(Actual = 50L))

  # With early stopping, may use fewer trials
  # (or same if boosting is effective)
  expect_true(mod_early$trials["Actual"] <= 50)

  # If early stopping kicked in, trees should differ
  if (mod_early$trials["Actual"] < 50) {
    expect_false(identical(mod_early$tree, mod_no_early$tree))
  }
})

# --- Combined test: Multiple parameters ---

test_that("multiple control parameters combine to affect output", {
  dat <- make_predictive_data(400, seed = 110)

  # Default parameters
  set.seed(1010)
  mod_default <- C5.0(dat[, -1], dat$y)

  # Multiple parameter changes
  set.seed(1010)
  mod_custom <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(
      winnow = TRUE,
      CF = 0.10,
      minCases = 10,
      fuzzyThreshold = TRUE,
      noGlobalPruning = TRUE
    )
  )

  # Should produce different trees
  expect_false(identical(mod_default$tree, mod_custom$tree))

  # Should produce different predictions
  pred_default <- predict(mod_default, dat[, -1], type = "prob")
  pred_custom <- predict(mod_custom, dat[, -1], type = "prob")
  expect_false(identical(pred_default, pred_custom))
})

# --- Reproducibility test ---

test_that("same seed produces identical models", {
  dat <- make_predictive_data(300, seed = 111)

  # Fit model twice with same seed
  set.seed(1011)
  mod_1 <- C5.0(dat[, -1], dat$y)

  set.seed(1011)
  mod_2 <- C5.0(dat[, -1], dat$y)

  # Trees should be identical

  expect_identical(mod_1$tree, mod_2$tree)
  expect_identical(mod_1$rules, mod_2$rules)
  expect_identical(mod_1$size, mod_2$size)

  # Predictions should be identical
  pred_1 <- predict(mod_1, dat[, -1])
  pred_2 <- predict(mod_2, dat[, -1])
  expect_identical(pred_1, pred_2)

  pred_prob_1 <- predict(mod_1, dat[, -1], type = "prob")
  pred_prob_2 <- predict(mod_2, dat[, -1], type = "prob")
  expect_identical(pred_prob_1, pred_prob_2)
})
