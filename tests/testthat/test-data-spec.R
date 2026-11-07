# Tests for passing data directly to the C code (R/data-spec.R, src/rdata.c)

make_spec_data <- function(n = 200, seed = 20261107) {
  set.seed(seed)
  x <- data.frame(
    a = rnorm(n),
    b = runif(n),
    f = factor(sample(c("lo", "mid", "hi"), n, TRUE)),
    o = factor(sample(letters[1:5], n, TRUE), ordered = TRUE)
  )
  x$a[sample(n, 15)] <- NA
  x$f[sample(n, 10)] <- NA
  lin <- 1.5 * ifelse(is.na(x$a), 0, x$a) + 2 * x$b + (x$o > "c")
  y2 <- factor(ifelse(lin + rnorm(n, sd = 0.7) > 1.5, "yes", "no"))
  y3 <- cut(
    lin + rnorm(n, sd = 0.7),
    c(-Inf, 0.8, 2, Inf),
    labels = c("A", "B", "C")
  )
  list(x = x, y2 = y2, y3 = y3, w = runif(n, 0.5, 2))
}

# Fit through both interfaces and compare the model text and the output
expect_same_as_legacy <- function(x, y, ...) {
  new_fit <- C5.0(x, y, ...)
  old_fit <- legacy_c50_fit(x, y, ...)
  expect_identical(strip_model_id(new_fit$tree), strip_model_id(old_fit$tree))
  expect_identical(
    strip_model_id(new_fit$rules),
    strip_model_id(old_fit$rules)
  )
  expect_identical(new_fit$output, strip_time_stamps(old_fit$output))
}

two_class <- function(y, v) {
  factor(ifelse(y == levels(y)[1], v[1], v[2]), levels = v)
}

# ------------------------------------------------------------------------------

test_that("the direct interface builds the same models as the text interface", {
  dat <- make_spec_data()
  ctrl <- C5.0Control(seed = 1)
  cst <- matrix(
    c(0, 3, 2, 0),
    2,
    dimnames = list(levels(dat$y2), levels(dat$y2))
  )

  expect_same_as_legacy(dat$x, dat$y2, control = ctrl)
  expect_same_as_legacy(dat$x, dat$y3, control = ctrl)
  expect_same_as_legacy(dat$x, dat$y3, rules = TRUE, control = ctrl)
  expect_same_as_legacy(dat$x, dat$y3, trials = 5, control = ctrl)
  expect_same_as_legacy(
    dat$x,
    dat$y2,
    trials = 5,
    rules = TRUE,
    weights = dat$w,
    control = ctrl
  )
  expect_same_as_legacy(dat$x, dat$y2, costs = cst, control = ctrl)
  expect_same_as_legacy(
    dat$x,
    dat$y3,
    control = C5.0Control(winnow = TRUE, subset = FALSE, seed = 1)
  )

  # Sampled fits also evaluate the hold-out cases, which are loaded a second
  # time; the hold-out subsample uses R's random numbers
  set.seed(1)
  new_fit <- C5.0(
    dat$x,
    dat$y3,
    control = C5.0Control(sample = 0.3, seed = 7)
  )
  set.seed(1)
  old_fit <- legacy_c50_fit(
    dat$x,
    dat$y3,
    control = C5.0Control(sample = 0.3, seed = 7)
  )
  expect_identical(strip_model_id(new_fit$tree), strip_model_id(old_fit$tree))
  expect_identical(new_fit$output, strip_time_stamps(old_fit$output))
  expect_match(new_fit$output, "Evaluation on test data", fixed = TRUE)
})

test_that("fitted objects store the attribute spec", {
  dat <- make_spec_data()
  fit <- C5.0(dat$x, dat$y3, weights = dat$w)

  expect_identical(fit$format_version, 2L)
  expect_identical(fit$spec$names, names(dat$x))
  expect_identical(fit$spec$discrete, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(fit$spec$ordered, c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(fit$spec$levels[[3]], levels(dat$x$f))
  expect_identical(fit$spec$class_levels, c("A", "B", "C"))
  expect_true(fit$spec$has_weights)
  expect_snapshot(cat(fit$names))
})

# ------------------------------------------------------------------------------
# Values and names that the text interface changed or could not handle

test_that("factor levels are used exactly as given", {
  set.seed(1)
  y <- factor(sample(c("a", "b"), 100, TRUE))
  level_sets <- list(
    c("?", "b"),
    c("N/A", "b"),
    c("a  b", "a b"),
    c(" a", "a"),
    c("", "z"),
    c("c,d", "a.b|x:y"),
    c("continuous", "ignore"),
    c("[ordered]x", "y")
  )
  for (lvls in level_sets) {
    x <- data.frame(f = two_class(y, lvls))
    fit <- C5.0(x, y)
    expect_identical(fit$spec$levels[[1]], lvls)
    new_x <- data.frame(f = factor(lvls, levels = lvls))
    expect_identical(
      as.character(predict(fit, new_x)),
      levels(y),
      info = paste(lvls, collapse = " / ")
    )
  }
})

test_that("class levels are used exactly as given", {
  set.seed(2)
  z <- rnorm(100)
  y <- factor(ifelse(z > 0, "p,q", "r: s"))
  fit <- C5.0(data.frame(z = z), y)
  expect_identical(fit$levels, c("p,q", "r: s"))
  expect_identical(
    as.character(predict(fit, data.frame(z = c(-2, 2)))),
    c("r: s", "p,q")
  )
})

test_that("unusual column names can be used", {
  set.seed(3)
  y <- factor(sample(c("a", "b"), 100, TRUE))
  signal <- ifelse(y == "a", 1, -1) + rnorm(100, sd = 0.3)
  nms <- c(
    "a:b, c",
    "case weight",
    "attributes excluded",
    "sample",
    "  spaces  ",
    strrep("x", 2000)
  )
  for (nm in nms) {
    x <- data.frame(signal)
    names(x) <- nm
    fit <- C5.0(x, y)
    expect_identical(fit$spec$names, nm, info = nm)
    expect_identical(C5predictors(fit), nm, info = nm)
    expect_gt(mean(predict(fit, x) == y), 0.9)
  }
})

test_that("a column named 'case weight' is a predictor when there are no weights", {
  set.seed(4)
  y <- factor(sample(c("a", "b"), 100, TRUE))
  x <- data.frame(
    `case weight` = ifelse(y == "a", 1, -1) + rnorm(100, sd = 0.3),
    check.names = FALSE
  )
  fit <- C5.0(x, y)
  expect_false(fit$spec$has_weights)
  expect_identical(C5predictors(fit), "case weight")
  expect_no_match(fit$output, "case weight attribute", fixed = TRUE)
})

test_that("predictors with a single value can be used", {
  set.seed(5)
  y <- factor(sample(c("a", "b"), 100, TRUE))
  x <- data.frame(
    z = ifelse(y == "a", 1, -1) + rnorm(100, sd = 0.3),
    f = factor(rep("continuous", 100)),
    ch = rep("ignore", 100),
    n = rep(1, 100)
  )
  fit <- C5.0(x, y)
  expect_identical(C5predictors(fit), "z")
  expect_identical(fit$spec$levels[[2]], "continuous")
})

test_that("character predictors use sorted levels without NA", {
  set.seed(6)
  y <- factor(sample(c("a", "b"), 100, TRUE))
  x <- data.frame(ch = sample(c("q", "B", "a", NA), 100, TRUE))
  fit <- C5.0(x, y)
  expect_identical(fit$spec$levels[[1]], c("B", "a", "q"))
})

test_that("missing class values are ignored", {
  dat <- make_spec_data()
  y <- dat$y2
  y[1:10] <- NA
  fit <- C5.0(dat$x, y)
  expect_match(fit$output, "Evaluation on training data (190 cases)", fixed = TRUE)
})

test_that("matrices and tibbles can be used", {
  dat <- make_spec_data()
  x_num <- dat$x[c("a", "b")]
  fit_df <- C5.0(x_num, dat$y2)
  fit_mat <- C5.0(as.matrix(x_num), dat$y2)
  expect_identical(fit_mat$tree, fit_df$tree)
  expect_identical(
    predict(fit_mat, as.matrix(x_num)),
    predict(fit_df, x_num)
  )
})

# ------------------------------------------------------------------------------
# Costs

test_that("costs between 0 and 1 are used", {
  dat <- make_spec_data()
  lvl <- levels(dat$y2)
  cst <- matrix(c(0, 1, 0.2, 0), 2, dimnames = list(lvl, lvl))
  fit <- C5.0(dat$x, dat$y2, costs = cst)
  fit_none <- C5.0(dat$x, dat$y2)

  expect_identical(fit$cost, "no, yes: 0.2\n")
  expect_match(fit$output, "Read misclassification costs", fixed = TRUE)
  expect_match(fit$tree, "costs=\"1\"", fixed = TRUE)
  expect_false(identical(predict(fit, dat$x), predict(fit_none, dat$x)))

  # the same model as a cost file with this value
  expect_same_as_legacy(dat$x, dat$y2, costs = cst)
})

test_that("zero costs can be used with three or more classes", {
  dat <- make_spec_data()
  lvl <- levels(dat$y3)
  cst <- matrix(1, 3, 3, dimnames = list(lvl, lvl))
  diag(cst) <- 0
  cst["A", "B"] <- 0
  fit <- C5.0(dat$x, dat$y3, costs = cst)
  expect_s3_class(predict(fit, dat$x), "factor")
})

test_that("cost matrices are matched to the class levels by name", {
  dat <- make_spec_data()
  lvl <- levels(dat$y3)
  cst <- matrix(c(0, 2, 4, 3, 0, 2, 5, 2, 0), 3, dimnames = list(lvl, lvl))
  perm <- c(3, 1, 2)
  cst_perm <- cst[perm, perm]

  fit <- C5.0(dat$x, dat$y3, costs = cst)
  fit_perm <- C5.0(dat$x, dat$y3, costs = cst_perm)
  expect_identical(fit_perm$costMatrix, cst)
  expect_identical(fit_perm$tree, fit$tree)
  expect_identical(predict(fit_perm, dat$x), predict(fit, dat$x))
})

test_that("costs and class levels with punctuation", {
  set.seed(7)
  z <- rnorm(100)
  y <- factor(ifelse(z + rnorm(100) > 0, "p,q", "r: s"))
  cst <- matrix(c(0, 3, 1, 0), 2, dimnames = list(levels(y), levels(y)))
  fit <- C5.0(data.frame(z = z), y, costs = cst)
  expect_match(fit$output, "Read misclassification costs", fixed = TRUE)
  expect_identical(levels(predict(fit, data.frame(z = z))), levels(y))
})

# ------------------------------------------------------------------------------
# Errors and warnings

test_that("bad predictors and labels give errors", {
  set.seed(8)
  n <- 20
  y <- factor(rep(c("a", "b"), n / 2))
  z <- rnorm(n)

  expect_snapshot(
    C5.0(data.frame(z, l = z > 0, d = Sys.Date() + seq_len(n)), y),
    error = TRUE
  )
  expect_snapshot(C5.0(data.frame(outcome = z), y), error = TRUE)
  expect_snapshot(
    C5.0(data.frame(z), y, control = C5.0Control(label = "z")),
    error = TRUE
  )
  expect_snapshot(
    C5.0(
      data.frame(`case weight` = z, check.names = FALSE),
      y,
      weights = rep(1, n)
    ),
    error = TRUE
  )
  expect_snapshot(
    C5.0(data.frame(z = replace(z, 2, Inf)), y),
    error = TRUE
  )
  expect_snapshot(
    C5.0(data.frame(z), y, weights = replace(rep(1, n), 3, -Inf)),
    error = TRUE
  )
  expect_snapshot(C5.0(data.frame(z), y, weights = rep(1, 3)), error = TRUE)
  expect_snapshot(C5.0(data.frame(z), y[1:5]), error = TRUE)
  expect_snapshot(
    C5.0(data.frame(z), factor(rep(NA_character_, n))),
    error = TRUE
  )
  expect_snapshot(
    C5.0(data.frame(f = addNA(factor(c(rep("u", n - 1), NA)))), y),
    error = TRUE
  )
  expect_snapshot(C5.0(data.frame(z, z, check.names = FALSE), y), error = TRUE)
})

test_that("bad cost matrices give errors", {
  dat <- make_spec_data()
  lvl <- levels(dat$y2)
  lvl3 <- levels(dat$y3)
  cost <- function(v, nms = lvl) {
    matrix(v, length(nms), dimnames = list(nms, nms))
  }
  wrong_names <- matrix(c(0, 2, 2, 0), 2, dimnames = list(lvl, c("no", "maybe")))

  expect_snapshot(C5.0(dat$x, dat$y2, costs = wrong_names), error = TRUE)
  expect_snapshot(C5.0(dat$x, dat$y2, costs = cost(c(0, 0, 2, 0))), error = TRUE)
  expect_snapshot(C5.0(dat$x, dat$y2, costs = cost(c(0, -1, 2, 0))), error = TRUE)
  expect_snapshot(C5.0(dat$x, dat$y2, costs = cost(c(0, NA, 2, 0))), error = TRUE)
  expect_snapshot(
    C5.0(dat$x, dat$y3, costs = cost(rep(0, 9), lvl3)),
    error = TRUE
  )
})

test_that("new factor levels in newdata are treated as missing", {
  dat <- make_spec_data()
  fit <- C5.0(dat$x, dat$y3)
  new_x <- dat$x[1:4, ]
  new_x$f <- as.character(new_x$f)
  new_x$f[1:2] <- c("new", "{odd}")
  new_x$o <- as.character(new_x$o)
  new_x$o[3] <- "z"

  expect_snapshot(pred <- predict(fit, new_x))

  missing_x <- new_x
  missing_x$f[1:2] <- NA
  missing_x$o[3] <- NA
  expect_identical(pred, predict(fit, missing_x))
})

test_that("bad columns in newdata give errors", {
  dat <- make_spec_data()
  fit <- C5.0(dat$x, dat$y3)
  new_x <- dat$x[1:4, ]

  expect_snapshot(
    predict(fit, transform(new_x, a = as.character(a))),
    error = TRUE
  )
  expect_snapshot(predict(fit, transform(new_x, b = Inf)), error = TRUE)

  # all-missing numeric columns are allowed, whatever their class
  all_na <- transform(new_x, a = NA)
  expect_identical(
    predict(fit, all_na),
    predict(fit, transform(new_x, a = NA_real_))
  )
})

# ------------------------------------------------------------------------------

test_that("fitted objects from version 0.2.0 still predict", {
  legacy <- readRDS(test_path("test_data", "legacy_fit_v020.rds"))
  for (nm in names(legacy$fits)) {
    fit <- legacy$fits[[nm]]
    expect_null(fit$format_version)
    expect_identical(
      predict(fit, legacy$newdata),
      legacy$expected[[nm]]$class,
      info = nm
    )
    if (!is.null(legacy$expected[[nm]]$prob)) {
      expect_identical(
        predict(fit, legacy$newdata, type = "prob"),
        legacy$expected[[nm]]$prob,
        info = nm
      )
    }
  }
  # Predictor names come from the `.names` text for these objects. That text
  # also lists the case weights, so `C5imp()` includes "case weight" for them
  # (as in version 0.2.0); new fits get the names from the attribute spec.
  expect_in(c("a", "f", "ch"), rownames(C5imp(legacy$fits$tree_weights)))
})

test_that("C5imp() lists only the predictors for new fits with weights", {
  dat <- make_spec_data()
  fit <- C5.0(dat$x, dat$y3, weights = dat$w)
  expect_setequal(rownames(C5imp(fit)), names(dat$x))
})

test_that("an outcome with one level gives a one-leaf model", {
  set.seed(1)
  x <- data.frame(a = rnorm(30), b = sample(letters[1:3], 30, replace = TRUE))
  y <- factor(rep("only", 30))

  fit <- C5.0(x, y)
  expect_equal(fit$size, 1)
  expect_equal(levels(predict(fit, x)), "only")
  expect_true(all(predict(fit, x) == "only"))
  prob <- predict(fit, x, type = "prob")
  expect_equal(dim(prob), c(30L, 1L))
  expect_equal(colnames(prob), "only")
  expect_true(all(prob == 1))

  rule_fit <- C5.0(x, y, rules = TRUE, trials = 3)
  expect_true(all(predict(rule_fit, x) == "only"))
})

test_that("model.frame() on a fit gives a standard model frame", {
  # ModTools does this to get the outcome of a fit. It needs the default
  # method, which evaluates the fit's call again
  dat <- data.frame(a = 1:20, y = factor(rep(c("u", "v"), each = 10)))
  fit <- C5.0(y ~ a, data = dat)
  fit$terms <- eval(fit$call$formula)
  mf <- model.frame(fit)
  expect_false(is.null(attr(mf, "terms")))
  expect_equal(unname(model.response(mf)), dat$y)
})
