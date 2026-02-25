# Integration smoke tests for basic C5.0 functionality

test_that("basic tree model execution (x/y interface)", {
  set.seed(1234)
  dat <- make_two_class_data(100, seed = 1234)

  mod <- C5.0(dat[, -1], y = dat$y)
  expect_s3_class(mod, "C5.0")
  expect_equal(mod$dims, c(100L, 3L))

  pred <- predict(mod, dat[, -1])
  expect_s3_class(pred, "factor")
  expect_length(pred, 100)
})

test_that("basic tree model execution (formula interface)", {
  set.seed(2345)
  dat <- make_two_class_data(100, seed = 2345)

  mod <- C5.0(y ~ ., data = dat)
  expect_s3_class(mod, "C5.0")
  expect_equal(mod$dims, c(100L, 3L))

  pred <- predict(mod, dat[, -1])
  expect_s3_class(pred, "factor")
  expect_length(pred, 100)
})

test_that("basic probability predictions work", {
  set.seed(3456)
  dat <- make_two_class_data(100, seed = 3456)

  mod <- C5.0(dat[, -1], dat$y)

  pred_prob <- predict(mod, dat[, -1], type = "prob")
  expect_true(is.matrix(pred_prob))
  expect_equal(nrow(pred_prob), 100)
  expect_equal(ncol(pred_prob), 2)
  expect_equal(colnames(pred_prob), c("A", "B"))
})

test_that("C5.0Control returns expected structure", {
  ctrl <- C5.0Control()
  expect_type(ctrl, "list")
  expect_named(
    ctrl,
    c(
      "subset",
      "bands",
      "winnow",
      "noGlobalPruning",
      "CF",
      "minCases",
      "fuzzyThreshold",
      "sample",
      "earlyStopping",
      "label",
      "seed"
    )
  )
})
