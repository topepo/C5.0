# C50 (development version)

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
