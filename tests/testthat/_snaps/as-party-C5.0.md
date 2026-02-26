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
      Error:
      ! For this model, 'subtree' should be between zero and 6

# plot.C5.0 warns when using trials instead of trial

    The option 'trials' was passed and will be ignored. 
          Did you mean to use 'trial'?

