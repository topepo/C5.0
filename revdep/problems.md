# ebmc

Version: 1.0.0

## In both

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# Pigengene

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in has_utility("pdfcrop") :
      pdfcrop not installed or not in PATH
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'Pigengene_inference.tex' failed.
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘AnnotationDbi’ ‘biomaRt’ ‘energy’ ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ':::' call which should be '::': ‘C50:::as.party.C5.0’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    Found the following possibly unsafe calls:
    File ‘Pigengene/R/bn.calculation.R’:
      assignInNamespace("supported.clusters", fixArgs, "bnlearn")
    
    check.pigengene.input: no visible global function definition for
      ‘setNames’
    welch.pvalue: no visible global function definition for ‘as.formula’
    welch.pvalue: no visible global function definition for ‘oneway.test’
    Undefined global functions or variables:
      as.formula oneway.test setNames
    Consider adding
      importFrom("stats", "as.formula", "oneway.test", "setNames")
    to your NAMESPACE file.
    ```

# plotmo

Version: 3.3.7

## In both

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# ssc

Version: 2.0.0

## In both

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

# StratifiedRF

Version: 0.2.2

## In both

*   checking PDF version of manual ... WARNING
    ```
    LaTeX errors when creating PDF version.
    This typically indicates Rd problems.
    ```

