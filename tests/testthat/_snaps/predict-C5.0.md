# predict errors on invalid type

    Code
      predict(mod, dat[, -1], type = "invalid")
    Condition
      Error:
      ! type should be either 'class' or 'prob'

# predict errors on prob with costs

    Code
      predict(mod, dat[, -1], type = "prob")
    Condition
      Error:
      ! confidence values (i.e. class probabilities) should 
                 not be used with costs

# predict errors on NULL newdata

    Code
      predict(mod, newdata = NULL)
    Condition
      Error:
      ! newdata must be non-null

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
      Error in `predict.C5.0()`:
      ! only one value of trials is allowed

# predict errors on non-positive trials

    Code
      predict(mod, dat[, -1], trials = 0)
    Condition
      Error:
      ! 'trials should be a positive integer

# predict warns when trials exceeds actual

    'trials' should be <= 1 for this object. Predictions generated using 1 trials

