# Tests for C5.0 core model training functions

# --- Basic Model Training ---

test_that("C5.0 trains basic tree model (x/y interface)", {
  set.seed(1001)
  dat <- make_two_class_data(100, seed = 1001)
  mod <- C5.0(dat[, -1], dat$y)

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$levels, c("A", "B"))
  expect_equal(mod$dims, c(100L, 3L))
  expect_false(mod$rbm)
  expect_equal(mod$trials["Requested"], c(Requested = 1L))
})

test_that("C5.0 trains basic tree model (formula interface)", {
  set.seed(1002)
  dat <- make_two_class_data(100, seed = 1002)
  mod <- C5.0(y ~ ., data = dat)

  expect_s3_class(mod, "C5.0")
  expect_true("Terms" %in% names(mod))
  expect_equal(mod$levels, c("A", "B"))
})

test_that("C5.0 trains rules model", {
  set.seed(1003)
  dat <- make_two_class_data(100, seed = 1003)
  mod <- C5.0(dat[, -1], dat$y, rules = TRUE)

  expect_s3_class(mod, "C5.0")
  expect_true(mod$rbm)
  expect_true(nchar(mod$rules) > 0)
})

test_that("C5.0 trains boosted model", {
  set.seed(1004)
  dat <- make_two_class_data(150, seed = 1004)
  mod <- C5.0(dat[, -1], dat$y, trials = 10)

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$trials["Requested"], c(Requested = 10L))
  expect_true(mod$trials["Actual"] >= 1)
})

test_that("C5.0 trains boosted rules model", {
  set.seed(1005)
  dat <- make_two_class_data(150, seed = 1005)
  mod <- C5.0(dat[, -1], dat$y, trials = 10, rules = TRUE)

  expect_s3_class(mod, "C5.0")
  expect_true(mod$rbm)
  expect_equal(mod$trials["Requested"], c(Requested = 10L))
})

test_that("C5.0 works with case weights", {
  set.seed(1006)
  dat <- make_two_class_data(100, seed = 1006)
  wts <- runif(100, 0.5, 2)
  mod <- C5.0(dat[, -1], dat$y, weights = wts)

  expect_s3_class(mod, "C5.0")
  expect_true(mod$caseWeights)
})

test_that("C5.0 works with cost matrix", {
  set.seed(1007)
  dat <- make_two_class_data(100, seed = 1007)
  costs <- make_cost_matrix(c("A", "B"))
  mod <- C5.0(dat[, -1], dat$y, costs = costs)

  expect_s3_class(mod, "C5.0")
  expect_true(nchar(mod$cost) > 0)
  expect_equal(mod$costMatrix, costs)
})

test_that("C5.0 works with multiclass outcome", {
  set.seed(1008)
  dat <- make_multi_class_data(150, n_classes = 4, seed = 1008)
  mod <- C5.0(dat[, -1], dat$y)

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$levels, c("A", "B", "C", "D"))
})

# --- C5.0Control Parameters ---

test_that("C5.0Control winnow parameter works", {
  set.seed(1009)
  dat <- make_two_class_data(100, seed = 1009)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(winnow = TRUE))

  expect_s3_class(mod, "C5.0")
  expect_true(mod$control$winnow)
})

test_that("C5.0Control CF parameter works", {
  set.seed(1010)
  dat <- make_two_class_data(100, seed = 1010)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(CF = 0.10))

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$control$CF, 0.10)
})

test_that("C5.0Control minCases parameter works", {
  set.seed(1011)
  dat <- make_two_class_data(100, seed = 1011)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(minCases = 5))

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$control$minCases, 5)
})

test_that("C5.0Control sample parameter works", {
  set.seed(1012)
  dat <- make_two_class_data(200, seed = 1012)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(sample = 0.5, seed = 42))

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$control$sample, 0.5)
})

test_that("C5.0Control bands parameter works with rules", {
  set.seed(1013)
  dat <- make_two_class_data(100, seed = 1013)
  mod <- C5.0(dat[, -1], dat$y, rules = TRUE, control = C5.0Control(bands = 5))

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$control$bands, 5)
})

test_that("C5.0Control noGlobalPruning parameter works", {
  set.seed(1014)
  dat <- make_two_class_data(100, seed = 1014)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(noGlobalPruning = TRUE))

  expect_s3_class(mod, "C5.0")
  expect_true(mod$control$noGlobalPruning)
})

test_that("C5.0Control fuzzyThreshold parameter works", {
  set.seed(1015)
  dat <- make_two_class_data(100, seed = 1015)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(fuzzyThreshold = TRUE))

  expect_s3_class(mod, "C5.0")
  expect_true(mod$control$fuzzyThreshold)
})

test_that("C5.0Control earlyStopping parameter works", {
  set.seed(1016)
  dat <- make_two_class_data(150, seed = 1016)
  mod <- C5.0(
    dat[, -1],
    dat$y,
    trials = 10,
    control = C5.0Control(earlyStopping = FALSE)
  )

  expect_s3_class(mod, "C5.0")
  expect_false(mod$control$earlyStopping)
})

test_that("C5.0Control strip_time_stamps removes timestamps from output", {
  set.seed(1017)
  dat <- make_two_class_data(100, seed = 1017)

  # With strip_time_stamps = TRUE (default), output should not contain timestamps
  mod_stripped <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(strip_time_stamps = TRUE)
  )
  expect_true(mod_stripped$control$strip_time_stamps)
  # Header should not have date/time
  expect_false(grepl("[A-Z][a-z]{2} [A-Z][a-z]{2} +\\d+", mod_stripped$output))
  # Should not have "Time:" line
  expect_false(grepl("^Time:.*secs", mod_stripped$output))

  # With strip_time_stamps = FALSE, output should contain timestamps
  set.seed(1018)
  mod_with_time <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(strip_time_stamps = FALSE)
  )
  expect_false(mod_with_time$control$strip_time_stamps)
  # Header should have date/time (e.g., "Thu Feb 26 11:05:49 2026")
  expect_true(grepl("[A-Z][a-z]{2} [A-Z][a-z]{2} +\\d+", mod_with_time$output))
})

# --- Error Conditions ---

test_that("C5.0 errors on non-factor outcome", {
  set.seed(2001)
  dat <- make_two_class_data(100, seed = 2001)
  expect_snapshot(
    error = TRUE,
    C5.0(dat[, -1], y = as.character(dat$y))
  )
})

test_that("C5.0 errors on missing column names", {
  set.seed(2002)
  dat <- make_two_class_data(100, seed = 2002)
  x <- as.matrix(dat[, -1])
  colnames(x) <- NULL
  expect_snapshot(
    error = TRUE,
    C5.0(x, dat$y)
  )
})

test_that("C5.0 errors on invalid cost matrix (not a matrix)", {
  set.seed(2003)
  dat <- make_two_class_data(100, seed = 2003)
  expect_snapshot(
    error = TRUE,
    C5.0(dat[, -1], dat$y, costs = c(1, 2, 3, 4))
  )
})

test_that("C5.0 errors on cost matrix dimension mismatch", {
  set.seed(2004)
  dat <- make_two_class_data(100, seed = 2004)
  bad_costs <- matrix(1, 3, 3)
  dimnames(bad_costs) <- list(LETTERS[1:3], LETTERS[1:3])
  expect_snapshot(
    error = TRUE,
    C5.0(dat[, -1], dat$y, costs = bad_costs)
  )
})

test_that("C5.0 errors on invalid trials value (too low)", {
  set.seed(2005)
  dat <- make_two_class_data(100, seed = 2005)
  expect_snapshot(
    error = TRUE,
    C5.0(dat[, -1], dat$y, trials = 0)
  )
})

test_that("C5.0 errors on invalid trials value (too high)", {
  set.seed(2006)
  dat <- make_two_class_data(100, seed = 2006)
  expect_snapshot(
    error = TRUE,
    C5.0(dat[, -1], dat$y, trials = 101)
  )
})

test_that("C5.0 errors on invalid x type", {
  set.seed(2007)
  dat <- make_two_class_data(100, seed = 2007)
  expect_snapshot(
    error = TRUE,
    C5.0(as.list(dat[, -1]), dat$y)
  )
})

test_that("C5.0 errors on non-numeric weights", {
  set.seed(2008)
  dat <- make_two_class_data(100, seed = 2008)
  expect_snapshot(
    error = TRUE,
    C5.0(dat[, -1], dat$y, weights = rep("a", 100))
  )
})

test_that("C5.0Control errors on invalid CF", {
  expect_snapshot(
    error = TRUE,
    C5.0Control(CF = 1.5)
  )
})

test_that("C5.0Control errors on invalid sample (too high)", {
  expect_snapshot(
    error = TRUE,
    C5.0Control(sample = 1.0)
  )
})

test_that("C5.0Control errors on invalid bands", {
  expect_snapshot(
    error = TRUE,
    C5.0Control(bands = 1)
  )
})

test_that("C5.0 errors when cost matrix missing row/col names", {
  set.seed(2009)
  dat <- make_two_class_data(100, seed = 2009)
  costs <- matrix(1, 2, 2)
  colnames(costs) <- c("A", "B")
  # rownames is NULL
  expect_snapshot(
    error = TRUE,
    C5.0(dat[, -1], dat$y, costs = costs)
  )
})

# --- Warning Conditions ---

test_that("C5.0 warns about rule banding without rules", {
  set.seed(3001)
  dat <- make_two_class_data(100, seed = 3001)
  expect_snapshot_warning(
    C5.0(dat[, -1], dat$y, control = C5.0Control(bands = 5))
  )
})

test_that("C5.0 warns about cost matrix without dimnames", {
  set.seed(3002)
  dat <- make_two_class_data(100, seed = 3002)
  costs <- matrix(1, 2, 2)
  costs[1, 2] <- 2
  expect_snapshot_warning(
    C5.0(dat[, -1], dat$y, costs = costs)
  )
})

# --- Print and Summary ---

test_that("print.C5.0 works for tree model", {
  set.seed(4001)
  dat <- make_two_class_data(100, seed = 4001)
  mod <- C5.0(dat[, -1], dat$y)
  expect_snapshot(print(mod))
})

test_that("print.C5.0 works for rules model", {
  set.seed(4002)
  dat <- make_two_class_data(100, seed = 4002)
  mod <- C5.0(dat[, -1], dat$y, rules = TRUE)
  expect_snapshot(print(mod))
})

test_that("print.C5.0 works for boosted model", {
  set.seed(4003)
  dat <- make_two_class_data(150, seed = 4003)
  mod <- C5.0(dat[, -1], dat$y, trials = 5)
  expect_snapshot(print(mod))
})

test_that("summary.C5.0 returns correct class", {
  set.seed(4004)
  dat <- make_two_class_data(100, seed = 4004)
  mod <- C5.0(dat[, -1], dat$y)
  summ <- summary(mod)

  expect_s3_class(summ, "summary.C5.0")
  expect_named(summ, c("output", "call"))
})

test_that("print.summary.C5.0 works", {
  set.seed(4005)
  dat <- make_two_class_data(100, seed = 4005)
  mod <- C5.0(dat[, -1], dat$y)
  summ <- summary(mod)

  # Capture output and check key components (avoid timestamp comparison)
  output <- capture.output(print(summ))
  expect_true(any(grepl("Call:", output)))
  expect_true(any(grepl("C5.0", output)))
  expect_true(any(grepl("Decision tree:", output) | grepl("Rule", output)))
})

# --- Additional Coverage Tests ---

test_that("C5.0 works with ordered factors", {
  set.seed(5001)
  n <- 150
  dat <- data.frame(
    y = factor(sample(c("low", "high"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = ordered(
      sample(c("small", "medium", "large"), n, replace = TRUE),
      levels = c("small", "medium", "large")
    )
  )
  mod <- C5.0(dat[, -1], dat$y)

  expect_s3_class(mod, "C5.0")
})

test_that("C5.0 handles missing values in predictors", {
  set.seed(5002)
  n <- 150
  dat <- make_two_class_data(n, seed = 5002)
  # Introduce some NAs

  dat$x1[sample(1:n, 10)] <- NA
  dat$x2[sample(1:n, 5)] <- NA

  mod <- C5.0(dat[, -1], dat$y)
  expect_s3_class(mod, "C5.0")

  # Predictions should also work with NAs
  pred <- predict(mod, dat[, -1])
  expect_length(pred, n)
})

test_that("C5.0 works with tibble input", {
  skip_if_not_installed("tibble")
  set.seed(5003)
  dat <- make_two_class_data(100, seed = 5003)
  dat_tbl <- tibble::as_tibble(dat)

  mod <- C5.0(dat_tbl[, -1], dat_tbl$y)
  expect_s3_class(mod, "C5.0")
})

test_that("C5.0 works with subset = FALSE", {
  set.seed(5004)
  dat <- make_two_class_data(100, seed = 5004)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(subset = FALSE))

  expect_s3_class(mod, "C5.0")
  expect_false(mod$control$subset)
})

test_that("C5.0 works with multiple control parameters combined", {
  set.seed(5005)
  n <- 200
  dat <- make_two_class_data(n, seed = 5005)
  mod <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(
      winnow = TRUE,
      fuzzyThreshold = TRUE,
      CF = 0.1,
      minCases = 5,
      seed = 123
    )
  )

  expect_s3_class(mod, "C5.0")
  expect_true(mod$control$winnow)
  expect_true(mod$control$fuzzyThreshold)
})

test_that("C5.0 works with asymmetric multiclass cost matrix", {
  set.seed(5006)
  dat <- make_multi_class_data(200, n_classes = 3, seed = 5006)

  costs <- matrix(1, 3, 3)
  costs[1, 2] <- 2
  costs[2, 3] <- 3
  costs[3, 1] <- 4
  dimnames(costs) <- list(c("A", "B", "C"), c("A", "B", "C"))

  mod <- C5.0(dat[, -1], dat$y, costs = costs)
  expect_s3_class(mod, "C5.0")
  expect_equal(mod$costMatrix, costs)
})

test_that("C5.0 works with larger dataset and more predictors", {
  set.seed(5007)
  n <- 500
  p <- 10
  x <- as.data.frame(matrix(rnorm(n * p), n, p))
  names(x) <- paste0("x", 1:p)
  # Add some factor predictors
  x$f1 <- factor(sample(letters[1:4], n, replace = TRUE))
  x$f2 <- factor(sample(LETTERS[1:3], n, replace = TRUE))
  y <- factor(sample(c("A", "B"), n, replace = TRUE))

  mod <- C5.0(x, y)
  expect_s3_class(mod, "C5.0")
  expect_equal(mod$dims[1], n)
  expect_equal(mod$dims[2], p + 2)
})

test_that("C5.0 boosted rules with bands works", {
  set.seed(5008)
  n <- 200
  dat <- make_two_class_data(n, seed = 5008)
  mod <- C5.0(
    dat[, -1],
    dat$y,
    trials = 5,
    rules = TRUE,
    control = C5.0Control(bands = 10)
  )

  expect_s3_class(mod, "C5.0")
  expect_true(mod$rbm)
})

test_that("C5.0 works with high minCases value", {
  set.seed(5009)
  n <- 300
  dat <- make_two_class_data(n, seed = 5009)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(minCases = 20))

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$control$minCases, 20)
})

test_that("C5.0 works with very low CF value", {
  set.seed(5010)
  dat <- make_two_class_data(150, seed = 5010)
  mod <- C5.0(dat[, -1], dat$y, control = C5.0Control(CF = 0.01))

  expect_s3_class(mod, "C5.0")
  expect_equal(mod$control$CF, 0.01)
})

test_that("C5.0 with sampling produces valid model", {
  set.seed(5011)
  n <- 300
  dat <- make_two_class_data(n, seed = 5011)
  mod <- C5.0(
    dat[, -1],
    dat$y,
    control = C5.0Control(sample = 0.7, seed = 42)
  )

  expect_s3_class(mod, "C5.0")
  # Model should still be able to make predictions
  pred <- predict(mod, dat[, -1])
  expect_length(pred, n)
})

test_that("C5.0 handles character predictors converted to factors", {
  set.seed(5012)
  n <- 100
  dat <- data.frame(
    y = factor(sample(c("A", "B"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = sample(c("cat", "dog", "bird"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  # Convert character to factor before modeling
  dat$x2 <- as.factor(dat$x2)

  mod <- C5.0(dat[, -1], dat$y)
  expect_s3_class(mod, "C5.0")
})

test_that("C5.0 with boosting and early stopping disabled runs more trials", {
  set.seed(5013)
  n <- 250
  dat <- make_two_class_data(n, seed = 5013)

  mod_early <- C5.0(
    dat[, -1],
    dat$y,
    trials = 20,
    control = C5.0Control(earlyStopping = TRUE)
  )

  mod_no_early <- C5.0(
    dat[, -1],
    dat$y,
    trials = 20,
    control = C5.0Control(earlyStopping = FALSE)
  )

  expect_s3_class(mod_early, "C5.0")
  expect_s3_class(mod_no_early, "C5.0")
  # Without early stopping, should use all requested trials
  expect_equal(mod_no_early$trials["Actual"], c(Actual = 20L))
})
