#' Predict new samples using a C5.0 model
#' 
#' This function produces predicted classes or confidence values
#'  from a C5.0 model.
#' 
#' Note that the number of trials in the object my be less than
#'  what was specified originally (unless `earlyStopping = FALSE`
#'  was used in [C5.0Control()]. If the number requested
#'  is larger than the actual number available, the maximum actual
#'  is used and a warning is issued.
#' 
#'   Model confidence values reflect the distribution of the classes
#'  in terminal nodes or within rules.
#' 
#'   For rule-based models (i.e. not boosted), the predicted
#'  confidence value is the confidence value from the most specific,
#'  active rule. Note that C4.5 sorts the rules, and uses the first
#'  active rule for prediction. However, the default in the original
#'  sources did not normalize the confidence values. For example,
#'  for two classes it was possible to get confidence values of
#'  (0.3815, 0.8850) or (0.0000, 0.922), which do not add to one.
#'  For rules, this code divides the values by their sum. The
#'  previous values would be converted to (0.3012, 0.6988) and (0,
#'  1). There are also cases where no rule is activated. Here, equal
#'  values are assigned to each class.
#' 
#'   For boosting, the per-class confidence values are aggregated
#'  over all of the trees created during the boosting process and
#'  these aggregate values are normalized so that the overall
#'  per-class confidence values sum to one.
#' 
#'   When the `cost` argument is used in the main function, class
#'  probabilities derived from the class distribution in the
#'  terminal nodes may not be consistent with the final predicted
#'  class. For this reason, requesting class probabilities from a
#'  model using unequal costs will throw an error.
#' 
#' @param object an object of class `C5.0`
#' @param newdata a matrix or data frame of predictors
#' @param trials an integer for how many boosting iterations are
#'  used for prediction. See the note below.
#' @param type either `"class"` for the predicted class or
#'  `"prob"` for model confidence values.
#' @param na.action when using a formula for the original model
#'  fit, how should missing values be handled?
#' @param \dots other options (not currently used)
#' @return when `type = "class"`, a factor vector is returned.
#'  When `type = "prob"`, a matrix of confidence values is returned
#'  (one column per class).
#' @author Original GPL C code by Ross Quinlan, R code and
#'  modifications to C by Max Kuhn, Steve Weston and Nathan Coulter
#' @seealso [C5.0()], [C5.0Control()],
#'  [summary.C5.0()], [C5imp()]
#' @references Quinlan R (1993). C4.5: Programs for Machine
#'  Learning. Morgan Kaufmann Publishers,
#'  \url{http://www.rulequest.com/see5-unix.html}
#' @keywords models
#' @examples
#' 
#' data(churn)
#' 
#' treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
#' predict(treeModel, head(churnTest[, -20]))
#' predict(treeModel, head(churnTest[, -20]), type = "prob")
#' 
#' 
#' @export
#' @method predict C5.0
#' @importFrom Cubist makeDataFile makeNamesFile QuinlanAttributes
predict.C5.0 <-
  function (object,
            newdata = NULL,
            trials = object$trials["Actual"],
            type = "class",
            na.action = na.pass,
            ...)  {
    if (!(type %in% c("class", "prob")))
      stop("type should be either 'class', 'confidence' or 'prob'", 
           call. = FALSE)
    if (object$cost != "" &
        type == "prob")
      stop("confidence values (i.e. class probabilities) should ", "
           not be used with costs",
           call. = FALSE)
    if (is.null(newdata))
      stop("newdata must be non-null", call. = FALSE)
    
    if (!is.null(object$Terms)) {
      object$Terms <- delete.response(object$Terms)
      newdata <-
        model.frame(object$Terms,
                    newdata,
                    na.action = na.action,
                    xlev = object$xlevels)
    } else
      newdata <- newdata[, object$predictors, drop = FALSE]
    
    if (is.null(colnames(newdata)))
      stop("column names are required", call. = FALSE)
    
    if (length(trials) > 1)
      stop("only one value of trials is allowed")
    if (trials > object$trials["Actual"])
      warning(
        paste(
          "'trials' should be <=",
          object$trials["Actual"],
          "for this object. Predictions generated using",
          object$trials["Actual"],
          "trials"
        ),
        call. = FALSE
      )
    
    ## If there are case weights used during training, the C code
    ## will expect a column of weights in the new data but the 
    ## values will be ignored. `makeDataFile` puts those last in 
    ## the data when `C5.0.default` is run, so we will add a 
    ## column of NA values at the end here
    if (object$caseWeights)
      newdata$case_weight_pred <- NA
    
    ## make cases file
    caseString <- makeDataFile(x = newdata, y = NULL)
    
    ## When passing trials to the C code, convert to
    ## zero if the original version of trials is used
    
    if (trials <= 0)
      stop("'trials should be a positive integer", call. = FALSE)
    if (trials == object$trials["Actual"])
      trials <- 0
    
    ## Add trials (not object$trials) as an argument
    Z <- .C(
      "predictions",
      as.character(caseString),
      as.character(object$names),
      as.character(object$tree),
      as.character(object$rules),
      as.character(object$cost),
      pred = integer(nrow(newdata)),
      confidence = double(length(object$levels) * nrow(newdata)),
      trials = as.integer(trials),
      output = character(1),
      PACKAGE = "C50"
    )
    if(any(grepl("Error limit exceeded", Z$output)))
      stop(Z$output, call. = FALSE)
    
    if (type == "class") {
      out <- factor(object$levels[Z$pred], levels = object$levels)
    } else {
      out <-
        matrix(Z$confidence,
               ncol = length(object$levels),
               byrow = TRUE)
      if (!is.null(rownames(newdata)))
        rownames(out) <- rownames(newdata)
      colnames(out) <- object$levels
    }
    out
  }
