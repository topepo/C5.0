# plot.C5.0 errors for rules model

    Code
      plot(mod)
    Condition
      Error in `plot()`:
      ! Only tree models can be plotted, not rule-based models.

# plot.C5.0 errors on invalid subtree

    Code
      plot(mod, subtree = 9999)
    Condition
      Error in `plot()`:
      ! `subtree` must be between 0 and 6, not 9999.

# plot.C5.0 warns when using trials instead of trial

    The option `trials` was passed and will be ignored.
    i Did you mean to use `trial`?

