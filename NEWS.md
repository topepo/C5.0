# C50 (development version)

* `C5.0()` and `predict()` now pass the data to the C code as R vectors. Before, the data were written as the text files of the command-line program (`.names`, `.data`, `.cases`, `.costs`) and parsed again by the C code. For data that the text files could hold, the models and predictions are the same as before. The text interface had these problems, which are now fixed:

  * Some values changed without a message: a factor level `"?"` became a missing value, a level `"N/A"` became "not applicable", and whitespace in names and levels was trimmed or collapsed, so levels such as `"a b"` and `"a  b"` were merged.
  * A column named `case weight` was used as case weights (and not as a predictor), even when no `weights` were given.
  * Factor levels with punctuation (e.g. `"c,d"`) were split into several levels, and the fit failed.
  * A predictor with a single value (e.g. a factor with one level) made the fit fail. A single level named `continuous`, `ignore`, `label`, or `date` was read as a type.
  * Column names that start with `attributes included` or `attributes excluded` were read as instructions, and names and values longer than 1000 characters gave an error.
  * Character predictors had `"NA"` as a level, and their levels were in the order of appearance. They now use sorted levels (in a locale-independent order). Models with character predictors can therefore differ slightly from before.

* Off-diagonal values of a cost matrix that are not greater than 1 are no longer dropped. Before, `C5.0()` passed only costs greater than 1 to the C code, so a cost of 0.5 (for example) had no effect. Models fitted with such costs will differ. Also, the rows of the cost matrix are now matched to the class levels by their row names; before, the row names were ignored and the rows were assumed to be in the order of the column names. See the new "Costs" section of `?C5.0` for how costs are used.

* Cost matrices are now checked before the fit: the values must be finite and non-negative, and with two classes the off-diagonal values must be greater than 0. (A zero cost with two classes makes the C code build a broken model that crashes in `predict()`.)

* `predict()` now treats factor or character values in `newdata` that were not in the training data as missing values, with a warning. Before, it stopped with an error ("bad value of ... Error limit exceeded") when the model was fitted with `x` and `y`.

* `C5.0()` now gives clear errors for logical or date predictors, infinite values in the predictors or `weights`, a predictor with the same name as the outcome label (`C5.0Control(label = )`), duplicated column names, an outcome with no levels, and `y` or `weights` with the wrong length.

* Fitted objects have two new elements: `spec` (the predictor names, types, and levels) and `format_version`. The `names` element (the text of the `.names` file) is still there for code that reads it, but it no longer has comment lines with the R version and a time stamp. Fitted objects from earlier versions can still be used with `predict()`; for them, the new data are still written as text with `Cubist::makeDataFile()`, so changes to that function in Cubist also affect predictions from those old objects.

* An outcome with a single level now gives a one-leaf model that predicts that level (with probability 1). In version 0.2.0 the C code failed but `C5.0()` still returned an object.

* The internal helper that `as.party()` uses to get the training data is no longer registered as a `model.frame()` method for C5.0 fits. `model.frame()` again uses the default method and returns a standard model frame, as it did in version 0.2.0. This fixes `model.response(model.frame(fit))`, which broke ModTools.

* For new fits with case weights, `C5imp()` no longer lists `case weight` as a predictor.

* `C5.0()` no longer limits `trials` to 100. Any whole number >= 1 is allowed; the C code already allocated its per-trial storage at run time, so the cap existed only in R. Models with 100 or fewer trials are unchanged. Note that some downstream packages have their own cap of 100: parsnip (`boost_tree()` C5.0 engine tuning range and `C5_rules()`), rules (`C5_rules` fit wrapper and tuning range), and caret (random search over `1:100`).

* `print.C5.0()` now says "no early stopping for boosting" when `earlyStopping = FALSE`. Before, it said "early stopping for boosting", which read as if early stopping were turned on.

* Added `strip_time_stamps` option to `C5.0Control()` (default `TRUE`) to remove timestamps from model output, making results reproducible across runs.

* Switched from base R `stop()` and `warning()` to cli's `cli_abort()` and `cli_warn()` for improved error messages with better formatting and context.

* Fixed compiler warnings for unused variables in C source code.

# C50 0.2.0

* Updated C code to stop overwriting keyword symbols. 

# C50 0.1.8

* Maintenance release to fix CRAN issues related to compiler issues.

* Updated GHA and tests

# C50 0.1.7

* Maintenance release to fix CRAN issues related to compiler warnings. 

# C50 0.1.6

* Maintenance release to fix CRAN issues related to compiler warnings. 


# C50 0.1.5

* Maintenance release to fix CRAN issues related to compiler warnings. 

# C50 0.1.4

* Maintenance release to fix CRAN issues by adding `rmarkdown` to Suggests.

* Fixed CRAN issues related to undefined behavior in the C code when requesting rules with `C5.0(rules = TRUE)`.


# C50 0.1.3

* Fixed CRAN issues for GCC 10 `-fno-common` flag.

* Added a `NEWS.md` file to track changes to the package.

* Removed `churn` data in favor of the version in the `modeldata` package. 
