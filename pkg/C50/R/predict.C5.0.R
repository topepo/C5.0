
predict.C5.0 <- function (object, newdata = NULL, trials = object$trials["Actual"], type = "class", na.action = na.pass, ...) 
{
  if(!(type %in% c("class", "prob"))) stop("type should be either 'class', 'confidence' or 'prob'")
  if(object$cost != "" & type == "prob") stop("confidence values (i.e. class probabilities) should not be used with costs")
  if(is.null(newdata)) stop("newdata must be non-null")
  
  if (!is.null(object$Terms))
    {
      object$Terms <- delete.response(object$Terms)
      newdata <- model.frame(object$Terms, newdata, na.action = na.action, xlev = object$xlevels) 
    } else newdata <- newdata[, object$predictors, drop = FALSE]


  if(length(trials) > 1) stop("only one value of trials is allowed")
  if(trials > object$trials["Actual"]) warning(paste("'trials' should be <=", object$trials["Actual"], "for this object. Predictions generated using", object$trials["Actual"], "trials"))

  ## If there are case weights used during training, the C code will expect a 
  ## column of rhtat in teh new data but the values will be ignored. `makeDataFile`
  ## puts those last in teh data when `C5.0.default` is run, so we will add
  ## a column of NA values at the end here
  if(object$caseWeights) newdata$case_weight_pred <- NA
  
  ## make cases file
  caseString <- makeDataFile(x = newdata, y = NULL)
  
  ## for testing
  ##cat(caseString, '\n')

  ## When passing trials to the C code, convert to
  ## zero if the original version of trials is used

  if(trials <= 0) stop("'trials should be a positive integer")
  if(trials == object$trials["Actual"]) trials <- 0

  ## Add trials (not object$trials) as an argument
  Z <- .C("predictions",
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

  if(type == "class")
    {
      out <- factor(object$levels[Z$pred], levels = object$levels)
    } else {
      out <- matrix(Z$confidence, ncol = length(object$levels), byrow= TRUE)
      if(!is.null(rownames(newdata))) rownames(out) <- rownames(newdata)
      colnames(out) <- object$levels 
    }
  out
}
