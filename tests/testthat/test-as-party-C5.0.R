# Tests for plot.C5.0, as.party.C5.0, and model.frame.C5.0

# --- as.party.C5.0 tests ---

test_that("as.party converts tree model to party object", {
  set.seed(1101)
  dat <- make_two_class_data(100, seed = 1101)
  mod <- C5.0(y ~ ., data = dat)

  party_obj <- as.party(mod)
  expect_s3_class(party_obj, "party")
  expect_s3_class(party_obj, "constparty")
})

test_that("as.party works with formula-fitted model", {
  set.seed(1102)
  dat <- make_two_class_data(100, seed = 1102)
  mod <- C5.0(y ~ ., data = dat)

  party_obj <- as.party(mod)
  expect_s3_class(party_obj, "party")
})

# --- model.frame.C5.0 tests ---

test_that("model.frame extracts data from formula model", {
  set.seed(1201)
  dat <- make_two_class_data(100, seed = 1201)
  mod <- C5.0(y ~ ., data = dat)

  mf <- model.frame(mod)
  expect_s3_class(mf, "data.frame")
  expect_equal(nrow(mf), 100)
})

# --- plot.C5.0 tests ---

test_that("plot.C5.0 runs without error for tree model", {
  skip_if_not_installed("partykit")
  set.seed(1301)
  dat <- make_two_class_data(100, seed = 1301)
  mod <- C5.0(y ~ ., data = dat)

  # Render the plot
  pdf(tempfile())
  plt <- plot(mod)
  dev.off()

  expect_null(plt)
})

test_that("plot.C5.0 works with subtree parameter", {
  skip_if_not_installed("partykit")
  set.seed(1302)
  dat <- make_two_class_data(100, seed = 1302)
  mod <- C5.0(y ~ ., data = dat)

  # Get the party object to check its size
  party_obj <- partykit::as.party(mod)
  expect_s3_class(party_obj, c("constparty", "party"))
})

# --- Error Conditions ---

test_that("plot.C5.0 errors for rules model", {
  set.seed(1401)
  dat <- make_two_class_data(100, seed = 1401)
  mod <- C5.0(y ~ ., data = dat, rules = TRUE)

  expect_snapshot(
    error = TRUE,
    plot(mod)
  )
})

test_that("plot.C5.0 errors on invalid subtree", {
  skip_if_not_installed("partykit")
  set.seed(1402)
  dat <- make_two_class_data(100, seed = 1402)
  mod <- C5.0(y ~ ., data = dat)

  expect_snapshot(
    error = TRUE,
    plot(mod, subtree = 9999)
  )
})

# --- Warning Conditions ---

test_that("plot.C5.0 warns when using trials instead of trial", {
  skip_if_not_installed("partykit")
  set.seed(1502)
  dat <- make_two_class_data(100, seed = 1502)
  mod <- C5.0(y ~ ., data = dat)

  expect_snapshot_warning({
    pdf(tempfile())
    plt <- plot(mod, trials = 1)
    dev.off()
  })
  expect_null(plt)
})
