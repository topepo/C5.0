\name{summary.C5.0}
\alias{summary.C5.0}
\title{
Summaries of C5.0 Models
}
\description{
This function prints out detailed summaries for C5.0 models.
}
\usage{
\method{summary}{C5.0}(object, ...)
}
\arguments{
  \item{object}{
an object of class \code{C5.0}
}
  \item{\dots}{
other options (not currently used)
}
}
\details{
The output of this function mirrors the output of the C5.0 command
line version.

The terminal nodes have text indicating the number of samples covered
by the node and the number that were incorrectly classified. Note
that, due to how the model handles missing values, the sample numbers
may be fractional.

There is a difference in the attribute usage numbers between this
output and the nominal command line output. Although the calculations
are almost exactly the same (we do not add 1/2 to everything), the C
code does not display that an attribute was used if the percentage of
training samples covered by the corresponding splits is very
low. Here, the threshold was lowered and the fractional usage is
shown.

}
\value{
A list with values
\item{output }{a single text string with the model output}
\item{comp2 }{the call to this function}
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
\code{\link{C5.0}}, \code{\link{C5.0Control}}, \code{\link{summary.C5.0}},  \code{\link{C5imp}}
}
\examples{
data(churn)

treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
summary(treeModel)


}

\keyword{ models }