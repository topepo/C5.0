# predict errors on invalid type

    Code
      predict(mod, dat[, -1], type = "invalid")
    Condition
      Error in `predict()`:
      ! `type` must be "class" or "prob", not a string.

# predict errors on prob with costs

    Code
      predict(mod, dat[, -1], type = "prob")
    Condition
      Error in `predict()`:
      ! Confidence values (i.e. class probabilities) should not be used with costs.

# predict errors on NULL newdata

    Code
      predict(mod, newdata = NULL)
    Condition
      Error in `predict()`:
      ! `newdata` must not be "NULL".

# predict errors on missing column names

    Code
      predict(mod, newdata)
    Condition
      Error in `newdata[, object$predictors, drop = FALSE]`:
      ! subscript out of bounds

# predict errors on multiple trials values

    Code
      predict(mod, dat[, -1], trials = c(1, 2, 3))
    Condition
      Error in `predict()`:
      ! `trials` must be a single value, not a vector of length 3.

# predict errors on non-positive trials

    Code
      predict(mod, dat[, -1], trials = 0)
    Condition
      Error in `predict()`:
      ! `trials` must be a positive integer, not a number.

# predict warns when trials exceeds actual

    `trials` should be <= 1 for this object.
    i Predictions generated using 1 trials.

