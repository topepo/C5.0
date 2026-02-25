# Tests for predict.C5.0 function

# --- Basic Predictions ---

test_that("predict returns class predictions from tree model", {
  set.seed(5001)
  dat <- make_two_class_data(100, seed = 5001)
  mod <- C5.0(dat[1:80, -1], dat$y[1:80])

  pred <- predict(mod, dat[81:100, -1])
  expect_s3_class(pred, "factor")
  expect_length(pred, 20)
  expect_equal(levels(pred), c("A", "B"))
})

test_that("predict returns probability predictions from tree model", {
  set.seed(5002)
  dat <- make_two_class_data(100, seed = 5002)
  mod <- C5.0(dat[1:80, -1], dat$y[1:80])

  pred <- predict(mod, dat[81:100, -1], type = "prob")
  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), 20)
  expect_equal(ncol(pred), 2)
  expect_equal(colnames(pred), c("A", "B"))
  # Probabilities should sum to 1
  row_sums <- rowSums(pred)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("predict works from rules model", {
  set.seed(5003)
  dat <- make_two_class_data(100, seed = 5003)
  mod <- C5.0(dat[1:80, -1], dat$y[1:80], rules = TRUE)

  pred_class <- predict(mod, dat[81:100, -1])
  expect_s3_class(pred_class, "factor")
  expect_length(pred_class, 20)

  pred_prob <- predict(mod, dat[81:100, -1], type = "prob")
  expect_true(is.matrix(pred_prob))
})

test_that("predict works from boosted model", {
  set.seed(5004)
  dat <- make_two_class_data(150, seed = 5004)
  mod <- C5.0(dat[1:100, -1], dat$y[1:100], trials = 5)

  pred <- predict(mod, dat[101:150, -1])
  expect_s3_class(pred, "factor")
  expect_length(pred, 50)
})

test_that("predict works with specific trials value", {
  set.seed(5005)
  dat <- make_two_class_data(150, seed = 5005)
  mod <- C5.0(dat[1:100, -1], dat$y[1:100], trials = 10)

  # Use trials <= actual to avoid warning
  actual_trials <- mod$trials["Actual"]
  if (actual_trials > 1) {
    pred <- predict(mod, dat[101:150, -1], trials = 1)
    expect_s3_class(pred, "factor")
    expect_length(pred, 50)
  } else {
    # Model early-stopped to 1 trial, just use default
    pred <- predict(mod, dat[101:150, -1])
    expect_s3_class(pred, "factor")
    expect_length(pred, 50)
  }
})

test_that("predict works from formula-fitted model", {
  set.seed(5006)
  dat <- make_two_class_data(100, seed = 5006)
  mod <- C5.0(y ~ ., data = dat[1:80, ])

  pred <- predict(mod, dat[81:100, ])
  expect_s3_class(pred, "factor")
  expect_length(pred, 20)
})

test_that("predict handles NA values via na.action", {
  set.seed(5007)
  dat <- make_two_class_data(100, seed = 5007)
  mod <- C5.0(y ~ ., data = dat[1:80, ])

  # Add some NAs to test data
  test_dat <- dat[81:100, ]
  test_dat$x1[1] <- NA

  pred <- predict(mod, test_dat, na.action = na.pass)
  expect_s3_class(pred, "factor")
  expect_length(pred, 20)
})

test_that("predict works with multiclass outcome", {
  set.seed(5008)
  dat <- make_multi_class_data(150, n_classes = 4, seed = 5008)
  mod <- C5.0(dat[1:100, -1], dat$y[1:100])

  pred_class <- predict(mod, dat[101:150, -1])
  expect_s3_class(pred_class, "factor")
  expect_equal(levels(pred_class), c("A", "B", "C", "D"))

  pred_prob <- predict(mod, dat[101:150, -1], type = "prob")
  expect_equal(ncol(pred_prob), 4)
  expect_equal(colnames(pred_prob), c("A", "B", "C", "D"))
})

test_that("predict works with case weights model", {
  set.seed(5009)
  dat <- make_two_class_data(100, seed = 5009)
  wts <- runif(80, 0.5, 2)
  mod <- C5.0(dat[1:80, -1], dat$y[1:80], weights = wts)

  pred <- predict(mod, dat[81:100, -1])
  expect_s3_class(pred, "factor")
  expect_length(pred, 20)
})

# --- Error Conditions ---

test_that("predict errors on invalid type", {
  set.seed(6001)
  dat <- make_two_class_data(100, seed = 6001)
  mod <- C5.0(dat[, -1], dat$y)

  expect_snapshot(
    error = TRUE,
    predict(mod, dat[, -1], type = "invalid")
  )
})

test_that("predict errors on prob with costs", {
  set.seed(6002)
  dat <- make_two_class_data(100, seed = 6002)
  costs <- make_cost_matrix(c("A", "B"))
  mod <- C5.0(dat[, -1], dat$y, costs = costs)

  expect_snapshot(
    error = TRUE,
    predict(mod, dat[, -1], type = "prob")
  )
})

test_that("predict errors on NULL newdata", {
  set.seed(6003)
  dat <- make_two_class_data(100, seed = 6003)
  mod <- C5.0(dat[, -1], dat$y)

  expect_snapshot(
    error = TRUE,
    predict(mod, newdata = NULL)
  )
})

test_that("predict errors on missing column names", {
  set.seed(6004)
  dat <- make_two_class_data(100, seed = 6004)
  mod <- C5.0(dat[, -1], dat$y)

  newdata <- as.matrix(dat[, -1])
  colnames(newdata) <- NULL
  expect_snapshot(
    error = TRUE,
    predict(mod, newdata)
  )
})

test_that("predict errors on multiple trials values", {
  set.seed(6005)
  dat <- make_two_class_data(100, seed = 6005)
  mod <- C5.0(dat[, -1], dat$y, trials = 5)

  expect_snapshot(
    error = TRUE,
    predict(mod, dat[, -1], trials = c(1, 2, 3))
  )
})

test_that("predict errors on non-positive trials", {
  set.seed(6006)
  dat <- make_two_class_data(100, seed = 6006)
  mod <- C5.0(dat[, -1], dat$y, trials = 5)

  expect_snapshot(
    error = TRUE,
    predict(mod, dat[, -1], trials = 0)
  )
})

# --- Warning Conditions ---

test_that("predict warns when trials exceeds actual", {
  set.seed(7001)
  dat <- make_two_class_data(150, seed = 7001)
  mod <- C5.0(dat[, -1], dat$y, trials = 5)

  expect_snapshot_warning(
    predict(mod, dat[, -1], trials = 100)
  )
})

# --- Additional Prediction Coverage Tests ---

test_that("predict handles data with missing values", {
  set.seed(7101)
  n <- 100
  dat <- make_two_class_data(n, seed = 7101)
  mod <- C5.0(dat[1:80, -1], dat$y[1:80])

  # Test data with NAs

  test_dat <- dat[81:100, -1]
  test_dat$x1[1:3] <- NA
  test_dat$x2[4:5] <- NA

  pred <- predict(mod, test_dat)
  expect_s3_class(pred, "factor")
  expect_length(pred, 20)
})

test_that("predict works with rules model and probabilities", {
  set.seed(7102)
  dat <- make_two_class_data(150, seed = 7102)
  mod <- C5.0(dat[1:100, -1], dat$y[1:100], rules = TRUE)

  pred_prob <- predict(mod, dat[101:150, -1], type = "prob")
  expect_true(is.matrix(pred_prob))
  expect_equal(nrow(pred_prob), 50)
  # Probabilities should sum to 1
  row_sums <- rowSums(pred_prob)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("predict works with boosted rules model", {
  set.seed(7103)
  dat <- make_two_class_data(200, seed = 7103)
  mod <- C5.0(dat[1:150, -1], dat$y[1:150], trials = 5, rules = TRUE)

  pred <- predict(mod, dat[151:200, -1])
  expect_s3_class(pred, "factor")
  expect_length(pred, 50)
})

test_that("predict preserves row names in probability output", {
  set.seed(7104)
  dat <- make_two_class_data(100, seed = 7104)
  mod <- C5.0(dat[1:80, -1], dat$y[1:80])

  test_dat <- dat[81:100, -1]
  rownames(test_dat) <- paste0("case_", 81:100)

  pred_prob <- predict(mod, test_dat, type = "prob")
  expect_equal(rownames(pred_prob), paste0("case_", 81:100))
})

test_that("predict works with ordered factor predictors", {
  set.seed(7105)
  n <- 150
  dat <- data.frame(
    y = factor(sample(c("low", "high"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = ordered(
      sample(c("small", "medium", "large"), n, replace = TRUE),
      levels = c("small", "medium", "large")
    )
  )
  mod <- C5.0(dat[1:100, -1], dat$y[1:100])

  pred <- predict(mod, dat[101:150, -1])
  expect_s3_class(pred, "factor")
})

test_that("predict works with model trained using sampling", {
  set.seed(7106)
  n <- 200
  dat <- make_two_class_data(n, seed = 7106)
  mod <- C5.0(
    dat[1:150, -1],
    dat$y[1:150],
    control = C5.0Control(sample = 0.7, seed = 42)
  )

  pred <- predict(mod, dat[151:200, -1])
  expect_s3_class(pred, "factor")
  expect_length(pred, 50)
})

test_that("predict works with model trained using winnowing", {
  set.seed(7107)
  n <- 200
  dat <- make_two_class_data(n, seed = 7107)
  mod <- C5.0(
    dat[1:150, -1],
    dat$y[1:150],
    control = C5.0Control(winnow = TRUE)
  )

  pred <- predict(mod, dat[151:200, -1])
  expect_s3_class(pred, "factor")
})

test_that("predict works with model trained using fuzzyThreshold", {
  set.seed(7108)
  n <- 200
  dat <- make_two_class_data(n, seed = 7108)
  mod <- C5.0(
    dat[1:150, -1],
    dat$y[1:150],
    control = C5.0Control(fuzzyThreshold = TRUE)
  )

  pred_class <- predict(mod, dat[151:200, -1])
  pred_prob <- predict(mod, dat[151:200, -1], type = "prob")

  expect_s3_class(pred_class, "factor")
  expect_true(is.matrix(pred_prob))
})
