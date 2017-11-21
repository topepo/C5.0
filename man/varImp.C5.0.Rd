\name{C5imp}
\alias{C5imp}
\title{
Variable Importance Measures for C5.0 Models
}
\description{
This function calculates the variable importance (aka attribute usage) for C5.0 models.
}
\usage{
C5imp(object, metric = "usage", pct = TRUE, ...)
}
\arguments{
  \item{object}{
an object of class \code{C5.0}
}
  \item{metric}{
either 'usage' or 'splits' (see Details below)
}
  \item{pct}{
a logical: should the importance values be converted to be between 0 and 100?
}
  \item{\dots}{
other options (not currently used)
}
}
\details{

By default, C5.0 measures predictor importance by determining the
percentage of training set samples that fall into all the terminal
nodes after the split (this is used when \code{metric = "usage"}). For
example, the predictor in the first split automatically has an
importance measurement of 100 percent. Other predictors may be used
frequently in splits, but if the terminal nodes cover only a handful
of training set samples, the importance scores may be close to
zero. The same strategy is applied to rule-based models as well as the
corresponding boosted versions of the model.

There is a difference in the attribute usage numbers between this
output and the nominal command line output. Although the calculations
are almost exactly the same (we do not add 1/2 to everything), the C
code does not display that an attribute was used if the percentage of
training samples covered by the corresponding splits is very
low. Here, the threshold was lowered and the fractional usage is
shown.

When \code{metric = "splits"}, the percentage of splits associated
with each predictor is calculated.

}
\value{
a data frame with a column \code{Overall} with the predictor usage values. The row names indicate the predictor.
}
\references{
Quinlan R (1993). C4.5: Programs for Machine Learning. Morgan Kaufmann Publishers,
\url{http://www.rulequest.com/see5-unix.html}
}
\author{
Original GPL C code by Ross Quinlan, R code and modifications to C by
Max Kuhn, Steve Weston and Nathan Coulter
}


\seealso{
\code{\link{C5.0}}, \code{\link{C5.0Control}}, \code{\link{summary.C5.0}},  \code{\link{predict.C5.0}}
}
\examples{
data(churn)

treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
C5imp(treeModel)
C5imp(treeModel, metric = "splits")
}

\keyword{ models }