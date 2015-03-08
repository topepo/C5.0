\name{plot.C5.0}
\alias{plot.C5.0}
\title{
Plot a decision tree
}
\description{
Plot a decision tree.
}
\usage{
\method{plot}{C5.0}(x, trial = 0, subtree = NULL, ...)
}

\arguments{
  \item{x}{
an object of class \code{C5.0}
}

  \item{trial}{
an integer for how many boosting iterations are used for prediction. NOTE: the internals of \code{C5.0} are zero-based so to get the initial decision tree you must use \code{trial = 0}. If \code{trial} is set too large, it is reset to the largest value and a warning is given.}

  \item{subtree}{
an optional integer that can be used to isolate nodes below the specified split. See \code{\link[partykit]{[.party}} for more details. 
}

  \item{...}{
options passed to \code{\link[partykit]{plot.party}}
}
}

\value{
No value is returned; a plot is rendered.
}
\references{
Quinlan R (1993). C4.5: Programs for Machine Learning. Morgan Kaufmann Publishers,
\url{http://www.rulequest.com/see5-unix.html}
}
\author{
Mark Culp, Max Kuhn
}


\seealso{
\code{\link{C5.0}}, \code{\link[partykit]{[.party}}
}
\examples{
mod1 <- C5.0(Species ~ ., data = iris)
plot(mod1)
plot(mod1, subtree = 3)


mod2 <- C5.0(Species ~ ., data = iris, trials = 10)
plot(mod2) ## should be the same as above

## plot first weighted tree
plot(mod2, trial = 1)
}

\keyword{ models }