test_that("basic execution", {
  skip_if_not_installed("modeldata")

  library(modeldata)
  set.seed(1)
  dat_tr <- sim_classification(100)
  dat_te <- sim_classification(100)

  set.seed(1)
  c5_xy <- C5.0(dat_tr[, -1], y = dat_tr$class)
  expect_snapshot(print(c5_xy))

  pred_xy <- predict(c5_xy, dat_te[, -1])
  expect_equal(length(pred_xy), nrow(dat_tr))

  pred_xy <- predict(c5_xy, dat_te[, -1], type = "prob")
  expect_equal(nrow(pred_xy), nrow(dat_tr))


  set.seed(2)
  c5_fm <- C5.0(class ~ ., data = dat_tr)
  expect_snapshot(print(c5_fm))

  pred_fm <- predict(c5_fm, dat_te[, -1])
  expect_equal(length(pred_fm), nrow(dat_tr))

  pred_fm <- predict(c5_fm, dat_te[, -1], type = "prob")
  expect_equal(nrow(pred_fm), nrow(dat_tr))
})
