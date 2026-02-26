# plot.C5.0 errors for rules model

    Code
      plot(mod)
    Condition
      Error:
      ! tree models only

# plot.C5.0 errors on invalid subtree

    Code
      plot(mod, subtree = 9999)
    Condition
      Error in `partynode()`:
      ! 'kids' must be an integer vector or a list of 'partynode' objects

# plot.C5.0 warns when using trials instead of trial

    The option 'trials' was passed and will be ignored. 
          Did you mean to use 'trial'?

