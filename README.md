
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/C50)](http://cran.r-project.org/web/packages/C50)
[![Downloads](http://cranlogs.r-pkg.org/badges/C50)](http://cran.rstudio.com/package=C50)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

The `C50` R package fits Quinlan's C5.0 classification model based on the source of from [`www.rulequest.com`](http://www.rulequest.com/see5-info.html). Some of the functionality is based on 

*  Quinlan. _C4.5: Programs For Machine Learning_ (1993b) Morgan Kaufmann Publishers Inc. San Francisco, CA

More details on C5.0 can be found in [_Applied Predictive Modeling_](http://appliedpredictivemodeling.com/). Details on the class probability computations can be found in this [blog post](http://appliedpredictivemodeling.com/blog/2015/9/8/c50-predicted-probability-shrinkage). 

To install the production version of the package, use:

```r
install.packages("C50")
```

and to install the development version, use

```r
require("devtools")
install_github("topepo/C5.0")
```




