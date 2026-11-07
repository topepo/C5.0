# fitted objects store the attribute spec

    Code
      cat(fit$names)
    Output
      
      outcome.
      
      outcome: A,B,C.
      a: continuous.
      b: continuous.
      f: hi,lo,mid.
      o: [ordered]a,b,c,d,e.
      case weight: continuous.

# bad predictors and labels give errors

    Code
      C5.0(data.frame(z, l = z > 0, d = Sys.Date() + seq_len(n)), y)
    Condition
      Error in `C5.0()`:
      ! Predictors must be numeric, factor, or character columns.
      x Unsupported columns: `l` (logical) and `d` (Date).
      i Convert logical and date columns to numbers or factors first.

---

    Code
      C5.0(data.frame(outcome = z), y)
    Condition
      Error in `C5.0()`:
      ! The outcome label "outcome" is also a column name in `x`.
      i Use `C5.0Control(label = )` to choose a different label.

---

    Code
      C5.0(data.frame(z), y, control = C5.0Control(label = "z"))
    Condition
      Error in `C5.0()`:
      ! The outcome label "z" is also a column name in `x`.
      i Use `C5.0Control(label = )` to choose a different label.

---

    Code
      C5.0(data.frame(`case weight` = z, check.names = FALSE), y, weights = rep(1, n))
    Condition
      Error in `C5.0()`:
      ! "case weight" cannot be a column name in `x` when `weights` are used.

---

    Code
      C5.0(data.frame(z = replace(z, 2, Inf)), y)
    Condition
      Error in `C5.0()`:
      ! `z` has infinite values.
      i Use `NA` for missing values instead.

---

    Code
      C5.0(data.frame(z), y, weights = replace(rep(1, n), 3, -Inf))
    Condition
      Error in `C5.0()`:
      ! `weights` has infinite values.
      i Use `NA` for missing values instead.

---

    Code
      C5.0(data.frame(z), y, weights = rep(1, 3))
    Condition
      Error in `C5.0()`:
      ! `weights` must have one value per row of `x` (20), not 3.

---

    Code
      C5.0(data.frame(z), y[1:5])
    Condition
      Error in `C5.0()`:
      ! `y` must have one value per row of `x` (20), not 5.

---

    Code
      C5.0(data.frame(z), factor(rep(NA_character_, n)))
    Condition
      Error in `C5.0()`:
      ! `y` must have at least one level.

---

    Code
      C5.0(data.frame(f = addNA(factor(c(rep("u", n - 1), NA)))), y)
    Condition
      Error in `C5.0()`:
      ! Factor levels must not be `NA`.
      x Column with a missing level: `f`.

---

    Code
      C5.0(data.frame(z, z, check.names = FALSE), y)
    Condition
      Error in `C5.0()`:
      ! Column names of `x` must be unique. Duplicated: "z".

# bad cost matrices give errors

    Code
      C5.0(dat$x, dat$y2, costs = wrong_names)
    Condition
      Error in `C5.0()`:
      ! The row and column names of `costs` must be the levels of `y`.
      i Levels of `y`: "no" and "yes".
      x Row names: "no" and "yes".
      x Column names: "no" and "maybe".

---

    Code
      C5.0(dat$x, dat$y2, costs = cost(c(0, 0, 2, 0)))
    Condition
      Error in `C5.0()`:
      ! With two classes, the off-diagonal elements of `costs` must be greater than zero.
      i Use a small positive value instead of zero.

---

    Code
      C5.0(dat$x, dat$y2, costs = cost(c(0, -1, 2, 0)))
    Condition
      Error in `C5.0()`:
      ! `costs` must contain only finite, non-negative values.
      i Missing, infinite, and negative costs are not allowed.

---

    Code
      C5.0(dat$x, dat$y2, costs = cost(c(0, NA, 2, 0)))
    Condition
      Error in `C5.0()`:
      ! `costs` must contain only finite, non-negative values.
      i Missing, infinite, and negative costs are not allowed.

---

    Code
      C5.0(dat$x, dat$y3, costs = cost(rep(0, 9), lvl3))
    Condition
      Error in `C5.0()`:
      ! At least one off-diagonal element of `costs` must be greater than zero.

# new factor levels in newdata are treated as missing

    Code
      pred <- predict(fit, new_x)
    Condition
      Warning in `predict()`:
      `newdata` has values that were not in the training set.
      i They are treated as missing.
      * f: "new", "{odd}"
      * o: "z"

# bad columns in newdata give errors

    Code
      predict(fit, transform(new_x, a = as.character(a)))
    Condition
      Error in `predict()`:
      ! Column `a` was numeric in the training set but is a character vector in `newdata`.

---

    Code
      predict(fit, transform(new_x, b = Inf))
    Condition
      Error in `predict()`:
      ! `b` has infinite values.
      i Use `NA` for missing values instead.

