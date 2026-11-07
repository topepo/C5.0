# lorax (0.2.0)

* GitHub: <https://github.com/tidymodels/lorax>
* Email: <mailto:max@posit.co>
* GitHub mirror: <https://github.com/cran/lorax>

Run `revdepcheck::revdep_details(, "lorax")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘spelling.R’
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       Error in `C5.0.default(x = m, y = y, weights = wt, ...)`: `y` must have at least two levels, not 1.
       Backtrace:
           ▆
        1. ├─C50::C5.0(y ~ x, data = small_data) at test-C5.0.R:851:3
        2. └─C50::C5.0.formula(y ~ x, data = small_data)
        3.   └─C50::C5.0.default(x = m, y = y, weights = wt, ...)
        4.     └─C50:::c50_data_spec(x, y, label = control$label, has_weights = !is.null(weights))
        5.       └─C50:::check_class_levels(y, call = call)
        6.         └─cli::cli_abort(...)
        7.           └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 66 | PASS 1861 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# ModTools (0.9.13)

* GitHub: <https://github.com/AndriSignorell/ModTools>
* Email: <mailto:andri@signorell.net>
* GitHub mirror: <https://github.com/cran/ModTools>

Run `revdepcheck::revdep_details(, "ModTools")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > r.nb <- FitMod(mdiab, data=d.pim$train, fitfn="naive_bayes")
     > r.lda <- FitMod(mdiab, data=d.pim$train, fitfn="lda")
     > r.qda <- FitMod(mdiab, data=d.pim$train, fitfn="qda")
     > r.lb <- FitMod(mdiab, data=d.pim$train, fitfn="lb")
     > 
     > mods <- list(glm=r.glm, rp=r.rp, rf=r.rf, svm=r.svm, c5=r.c5
     +              , nn=r.nn, nb=r.nb, lda=r.lda, qda=r.qda, lb=r.lb)
     > 
     > # insight in the Regression tree
     > plot(r.rp, box.palette = as.list(Pal("Helsana", alpha = 0.5)))
     > 
     > # Insample accuracy ...
     > TModC(mods, ord="auc")
     Setting levels: control = neg, case = pos
     Setting direction: controls < cases
     Setting levels: control = neg, case = pos
     Setting direction: controls < cases
     Setting levels: control = neg, case = pos
     Setting direction: controls < cases
     Setting levels: control = neg, case = pos
     Setting direction: controls < cases
     Error in if (attr(attr(data, "terms"), "response")) { : 
       argument is of length zero
     Calls: TModC ... FUN -> roc -> roc.default -> Response -> model.response
     Execution halted
     ```

