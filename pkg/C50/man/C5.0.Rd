\name{C5.0.default}
\alias{C5.0.default}
\alias{C5.0.formula}
\alias{C5.0}
\title{
C5.0 Decision Trees and Rule-Based Models
}
\description{
Fit classification tree models or rule-based models using Quinlan's
C5.0 algorithm
}
\usage{
C5.0(x, ...)

\method{C5.0}{default}(x, y, trials = 1, rules= FALSE, 
     weights = NULL, 
     control = C5.0Control(), 
     costs = NULL, ...)


\method{C5.0}{formula}(formula, data, weights, subset,
     na.action = na.pass, ...)
}

\arguments{
  \item{x}{
a data frame or matrix of predictors.
}
  \item{y}{
a factor vector with 2 or more levels
}
  \item{trials}{
an integer specifying the number of boosting iterations. A value of
one indicates that a single model is used.
}
  \item{rules}{
A logical: should the tree be decomposed into a rule-based model?
}
  \item{weights}{
an optional numeric vector of case weights. Note that the data used for the case weights will not be used as a splitting variable in the model (see \url{http://www.rulequest.com/see5-win.html#CASEWEIGHT} for Quinlan's notes on case weights).} 
  \item{control}{
a list of control parameters; see \code{\link{C5.0Control}}
}
  \item{costs}{
a matrix of costs associated with the possible errors. The matrix
should have C columns and rows where C is the number of class levels.
}
  \item{formula}{
a formula, with a response and at least one predictor.
}
  \item{data}{
an optional data frame in which to interpret the variables named in the formula.
}
  \item{subset}{
optional expression saying that only a subset of the rows of the data should be used in the fit.
}
  \item{na.action}{
a function which indicates what should happen when the data contain \code{NA}s. The default is to include missing values since the model can accommodate them. 
}

  \item{\dots}{
other options to pass into the function (not currently used with default method)
}
}
\details{

This model extends the C4.5 classification algorithms described in
Quinlan (1992). The details of the extensions are largely
undocumented. The model can take the form of a full decision tree or a
collection of rules (or boosted versions of either).

When using the formula method, factors and other classes are preserved
(i.e. dummy variables are not automatically created). This particular
model handles non-numeric data of some types (such as character,
factor and ordered data).

The cost matrix should by CxC, where C is the number of
classes. Diagonal elements are ignored. Columns should correspond to
the true classes and rows are the predicted classes. For example, if C
= 3 with classes Red, Blue and Green (in that order), a value of 5 in
the (2,3) element of the matrix would indicate that the cost of
predicting a Green sample as Blue is five times the usual value (of
one). Note that when costs are used, class probabilities cannot be
generated using \code{\link{predict.C5.0}}.

Internally, the code will attempt to halt boosting if it appears to be
ineffective. For this reason, the value of \code{trials} may be
different from what the model actually produced. There is an option to
turn this off in \code{\link{C5.0Control}}.

}
\value{
An object of class \code{C5.0} with elements:

  \item{boostResults}{
a parsed version of the boosting table(s) shown in the output
}
  \item{call}{
the function call
}
  \item{caseWeights}{
not currently supported.
}
  \item{control}{
an echo of the specifications from \code{\link{C5.0Control}}
}
  \item{cost}{
the text version of the cost matrix (or "")
}
  \item{costMatrix}{
an echo of the model argument
}
  \item{dims}{
original dimensions of the predictor matrix or data frame
}
  \item{levels}{
a character vector of factor levels for the outcome
}
  \item{names}{
a string version of the names file
}
  \item{output}{
a string version of the command line output
}
  \item{predictors}{
a character vector of predictor names
}
  \item{rbm}{
a logical for rules
}
  \item{rules}{
a character version of the rules file
}
  \item{size}{
n integer vector of the tree/rule size (or sizes in the case of boosting)
}
  \item{tree}{
a string version of the tree file
}
  \item{trials}{
a named vector with elements \code{Requested} (an echo of the function call) and \code{Actual} (how many the model used)
}

}
\references{
Quinlan R (1993). C4.5: Programs for Machine Learning. Morgan Kaufmann Publishers,
\url{http://www.rulequest.com/see5-unix.html}
}
\author{
Original GPL C code by Ross Quinlan, R code and modifications to C by
Max Kuhn, Steve Weston and Nathan Coulter
}
\note{
The command line version currently supports more data types than the R
port. Currently, numeric, factor and ordered factors are allowed as
predictors.
}

\seealso{
\code{\link{C5.0Control}}, \code{\link{summary.C5.0}}, \code{\link{predict.C5.0}}, \code{\link{C5imp}}
}
\examples{
data(churn)

treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
treeModel
summary(treeModel)

ruleModel <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)
ruleModel
summary(ruleModel)
}


\keyword{ models }
