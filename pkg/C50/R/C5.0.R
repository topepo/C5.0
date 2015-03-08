C5.0 <-  function(x, ...) UseMethod("C5.0")

C5.0.default <- function(x, y,
                         trials = 1,
                         rules = FALSE,
                         weights = NULL,
                         control = C5.0Control(),
                         costs = NULL,
                         ...)
{
  funcCall <- match.call(expand.dots = TRUE)
  if(!is.factor(y)) stop("C5.0 models require a factor outcome")

  if(control$bands > 2 & !rules)
    {
      warning("rule banding only works with rules; 'rules' was changed to TRUE")
      rules <- TRUE
    }
  
  ## to do add weightings
  
  lvl <- levels(y)
  nClass <- length(lvl)
  if(!is.null(costs))
    {
      if(!is.matrix(costs)) stop("'costs' should be a matrix")
      if(ncol(costs) != nClass | nrow(costs) != nClass)
        stop(paste("'cost should be a ", nClass, "x", nClass,
                   "matrix", sep = ""))
      if(is.null(dimnames(costs)))
        {
          warning("\nno dimnames were given for the cost matrix; the factor levels will be used\n")
          colnames(costs) <- lvl
          rownames(costs) <- lvl
        } else {
          if(is.null(colnames(costs)) | is.null(rownames(costs))) stop("both row and column names are needed")    
        }
      costString <- makeCostFile(costs)
    } else costString <- ""

  maxtrials <- 100
  if(trials < 1 | trials > maxtrials)
    stop(paste("number of boosting iterations must be between 1 and" ,maxtrials))

  if(!is.data.frame(x) & !is.matrix(x)) stop("x must be a matrix or data frame")

  if(!is.null(weights) && !is.numeric(weights))
    stop("case weights must be numeric")
  
  ## TODO: add case weights to these files when needed
  namesString <- makeNamesFile(x, y, w = weights, label = control$label, comments = TRUE)
  dataString <- makeDataFile(x, y, weights)

  Z <- .C("C50",
          as.character(namesString),
          as.character(dataString),
          as.character(costString),
          as.logical(control$subset),       # -s "use the Subset option" var name: SUBSET
          as.logical(rules),                # -r "use the Ruleset option" var name: RULES
          
          ## for the bands option, I'm not sure what the default should be.
          as.integer(control$bands),        # -u "sort rules by their utility into bands" var name: UTILITY
          
          ## The documentation has two options for boosting:
          ## -b use the Boosting option with 10 trials
          ## -t trials ditto with specified number of trial
          ## I think we should use -t
          as.integer(trials),               # -t : " ditto with specified number of trial", var name: TRIALS

          as.logical(control$winnow),       # -w "winnow attributes before constructing a classifier" var name: WINNOW 
          as.double(control$sample),        # -S : use a sample of x% for training
                                        #      and a disjoint sample for testing var name: SAMPLE
          as.integer(control$seed),         # -I : set the sampling seed value
          as.integer(control$noGlobalPruning),
                                        # -g: "turn off the global tree pruning stage" var name: GLOBAL
          as.double(control$CF),            # -c: "set the Pruning CF value" var name: CF

          ## Also, for the number of minimum cases, I'm not sure what the
          ## default should be. The code looks like it dynamically sets the
          ## value (as opposed to a static, universal integer
          as.integer(control$minCases),     # -m : "set the Minimum cases" var name: MINITEMS

          as.logical(control$fuzzyThreshold),
                                        # -p "use the Fuzzy thresholds option" var name: PROBTHRESH     
          as.logical(control$earlyStopping), # toggle C5.0 to check to see if we should stop boosting early
          ## the model is returned in 2 files: .rules and .tree
          tree = character(1),             # pass back C5.0 tree as a string
          rules = character(1),            # pass back C5.0 rules as a string
          output = character(1),           # get output that normally goes to screen
          PACKAGE = "C50"
          )

  ## Figure out how may trials were actually used. 
  modelContent <- strsplit(if(rules) Z$rules else Z$tree, "\n")[[1]]
  entries <- grep("^entries", modelContent, value = TRUE)
  if(length(entries) > 0)
    {
      actual <- as.numeric(substring(entries, 10, nchar(entries)-1))
    } else actual <- trials

  if(trials > 1)
    {
      boostResults <- getBoostResults(Z$output)
      ## This next line is here to avoid a false positive warning in R
      ## CMD check:
      ## * checking R code for possible problems ... NOTE
      ## C5.0.default: no visible binding for global variable 'Data'
      Data <- NULL
      size <- if(!is.null(boostResults)) subset(boostResults, Data == "Training Set")$Size else NA
    }   else {
    boostResults <- NULL
    size <- length(grep("[0-9])$", strsplit(Z$output, "\n")[[1]]))
  }
  
  out <- list(names = namesString,
              cost = costString,
              costMatrix = costs,
              caseWeights = !is.null(weights),
              control = control,
              trials = c(Requested = trials, Actual = actual),
              rbm = rules,
              boostResults = boostResults,
              size = size, 
              dims = dim(x),
              call = funcCall,
              levels = levels(y),
              output = Z$output,
              tree = Z$tree,
              predictors = colnames(x),
              rules = Z$rules)

  class(out) <- "C5.0"
  out
}

C5.0.formula <- function (formula, data, weights, subset, na.action = na.pass, ...) 
{
  call <- match.call()

  m <- match.call(expand.dots = FALSE)
  m$rules <- m$trails <- m$control <- m$cost <- m$... <- NULL
  m$na.action <- na.action
  m[[1L]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  
  y <- model.extract(m, "response")
  wt <- model.extract(m, "weights")
  if (length(wt) == 0L)  wt <- NULL
  m <- m[,-1,drop = FALSE]
  out <- C5.0.default(x = m, y = y, weights = wt, ...)
  out$call <- call
  out$Terms <- Terms
  out$xlevels <- .getXlevels(Terms, m)
  out

}


C5.0Control <- function(subset = TRUE,    ## in C, equals  SUBSET=0,	/* subset tests allowed */
                        bands = 0,
                        winnow = FALSE,
                        noGlobalPruning = FALSE,
                        CF = 0.25,
                        minCases = 2,
                        fuzzyThreshold = FALSE,
                        sample = 0.0,
                        seed = sample.int(4096, size=1) - 1L,
                        earlyStopping = TRUE,
                        label = "outcome")
  {
    if(CF < 0 | CF > 1)
      stop("confidence level must between 0 and 1")
    if(sample < 0.0 | sample > .999)
      stop("sampling percentage must be between 0.0 and .999")

    if(bands == 1 | bands > 10000)
      stop("if used, bands must be between 2 and 10000")
    
    list(subset = subset,
         bands = bands,
         winnow = winnow,
         noGlobalPruning = noGlobalPruning,
         CF = CF,
         minCases = minCases,
         fuzzyThreshold = fuzzyThreshold,
         sample = sample,
         earlyStopping = earlyStopping,
         label = label,
         seed = seed %% 4096L)
  }


print.C5.0 <- function(x, ...)
  {
    cat("\nCall:\n", truncateText(deparse(x$call, width.cutoff = 500)), "\n\n", sep = "")

    if(x$rbm) cat("Rule-Based Model\n") else cat("Classification Tree\n")
    
    cat("Number of samples:", x$dims[1],
        "\nNumber of predictors:", x$dims[2],
        "\n\n")

    if(x$trials["Requested"] > 1)
      {        
        if(x$trials[1] == x$trials[2])
          {
            cat("Number of boosting iterations:", x$trials["Requested"], "\n")
          } else {
            cat("Number of boosting iterations:", x$trials["Requested"],
                "requested; ", x$trials["Actual"],
                "used due to early stopping\n")
          }
        if(!all(is.na(x$size))) cat(ifelse(x$rbm, "Average number of rules:", "Average tree size:"), round(mean(x$size, na.rm = TRUE), 1), "\n\n") else cat("\n")
      } else cat(ifelse(x$rbm, "Number of Rules:", "Tree size:"), x$size, "\n\n")

    otherOptions <- NULL
    if(x$control$subset) otherOptions <- c(otherOptions, "attempt to group attributes")   
    if(x$control$winnow) otherOptions <- c(otherOptions, "winnowing")
    if(x$control$noGlobalPruning) otherOptions <- c(otherOptions, "no global pruning")
    if(x$control$CF != 0.25) otherOptions <- c(otherOptions,
         paste("confidence level: ", x$control$CF, sep = ""))
    if(x$control$minCases != 2) otherOptions <- c(otherOptions,
         paste("minimum number of cases: ", x$control$minCases, sep = ""))
    if(x$control$fuzzyThreshold) otherOptions <- c(otherOptions, "fuzzy thresholds")    
    if(x$control$bands > 0) otherOptions <- c(otherOptions,
                                              paste(x$control$bands, " utility bands", sep = ""))
    if(!x$control$earlyStopping & x$trials["Requested"] > 1) otherOptions <- c(otherOptions, "early stopping for boosting")
    if(x$control$sample > 0) otherOptions <- c(otherOptions,
                                               paste(round(100*x$control$sample, 1), "% sub-sampling", sep = ""))    
    if(!is.null(otherOptions))
      {
        cat(truncateText(paste("Non-standard options:", paste(otherOptions, collapse = ", "))))
        cat("\n\n")
      }
    

    if(x$cost != "")
      {
        cat("Cost Matrix:\n")
        print(x$costMatrix)
      }
    
    output <- strsplit(x$output, "\n")[[1]]
    sizeIndex <- grep("^\t.*Size", output)
    if(length(sizeIndex) > 0 & FALSE)
      {
        out <- strsplit(output[sizeIndex+2], " ")[[1]]
        out <- out[!(out %in% c("\t", ""))]
        out <- out[!grepl("[[:punct:]]", out)]
        if(length(out) > 0) cat("Tree Size: ", out, "\n")
      }
  }


summary.C5.0 <- function(object, ...)
  {
    out <- list(output = object$output, call = object$call)
    class(out) <- "summary.C5.0"
    out
  }

print.summary.C5.0 <- function(x, ...)
  {
    cat("\nCall:\n", truncateText(deparse(x$call, width.cutoff = 500)), "\n\n", sep = "")
    cat(x$output)
    cat("\n")
    invisible(x) 
  }

truncateText <- function(x)
  {
    if(length(x) > 1) x <- paste(x, collapse = "")
    w <- options("width")$width
    if(nchar(x) <= w) return(x)

    cont <- TRUE
    out <- x
    while(cont)
      {
        
        tmp <- out[length(out)]
        tmp2 <- substring(tmp, 1, w)
        
        spaceIndex <- gregexpr("[[:space:]]", tmp2)[[1]]
        stopIndex <- spaceIndex[length(spaceIndex) - 1] - 1
        tmp <- c(substring(tmp2, 1, stopIndex),
                 substring(tmp, stopIndex + 1))
        out <- if(length(out) == 1) tmp else c(out[1:(length(x)-1)], tmp)
        if(all(nchar(out) <= w)) cont <- FALSE
      }

    paste(out, collapse = "\n")
  }

C5imp <- function(object, metric = "usage", pct = TRUE, ...)
  {
    if(!(metric %in% c("usage", "splits"))) stop("metric should be either 'usage' or 'splits'")
    allVar <- getOriginalVars(object)
    allVar <- gsub("\\", "", allVar, fixed = TRUE)
    if(metric == "usage")
      {
        object$output <- strsplit( object$output, "\n")[[1]]
        usageIndex <- grep("Attribute usage:", object$output, fixed = TRUE)
        if(length(usageIndex) == 0) stop("Error in parsing model output")
        object$output <- object$output[usageIndex:length(object$output)]
        usageData <- grep("%\t", object$output, fixed = TRUE, value = TRUE)

        usageData <- strsplit(usageData, "%", fixed = TRUE)
        if(!all(unlist(lapply(usageData, length)) == 2)) stop("Error in parsing model output")

        usageData <- lapply(usageData, function(x) gsub("[[:blank:]]", "", x))
        usageData <- as.data.frame(do.call("rbind", usageData), stringsAsFactors = FALSE)
        elim <- allVar[!(allVar %in% usageData$V2)]
        if(length(elim) > 0)
          {
            elimVars <- data.frame(V1 = 0, V2 = elim, stringsAsFactors = FALSE)
            usageData <- rbind(usageData, elimVars) 
          }
        out <- data.frame(Overall =  as.numeric(as.character(usageData$V1)))
        rownames(out) <-  usageData$V2
      } else {
        varData <- strsplit(paste(object$tree, object$rules), "\n")[[1]]
        varData <- grep("att=", varData, value = TRUE)
        varData <- breakUp(varData)
        varData <- unlist(lapply(varData, function(x) x["att"]))
        
        varData <- as.data.frame(table(varData), stringsAsFactors = FALSE)
        elim <- allVar[!(allVar %in% varData$varData)]
        if(length(elim) > 0)
          {
            elimVars <- data.frame(varData = elim, Freq = 0, stringsAsFactors = FALSE)
            varData <- rbind(varData, elimVars) 
          }
        out <- data.frame(Overall =  as.numeric(as.character(varData$Freq)))
        if(pct) out$Overall <- out$Overall/sum(out$Overall)*100
        rownames(out) <-  varData$varData
      }
    out[order(out$Overall, decreasing = TRUE),,drop = FALSE]
  }

breakUp <- function(y)
  {
    y <- gsub("\"", "", y)
    y <- strsplit(y, " ", fixed = TRUE)
    y <- lapply(y,
                function(z)
                {
                  z <- strsplit(z, "=", fixed = TRUE)
                  nms <- unlist(lapply(z, function(a) a[1]))
                  val <- unlist(lapply(z, function(a) a[2]))
                  names(val) <- nms
                  val
                })
   y
  }

getOriginalVars <- function(x)
  {
    treeDat <- strsplit(x$names, "\n")[[1]]
    varStart <- grep(paste(x$control$label, ":", sep = ""),
                     treeDat)
    if(length(varStart) == 0) stop("cannot parse names file")
    treeDat <- treeDat[(varStart+1):length(treeDat)]
    treeDat <- strsplit(treeDat, ":")
    unlist(lapply(treeDat, function(x) x[1]))
  }

getVars <- function(x)
{
  ## One of these is always ""
  treeDat <- paste(x$tree, x$rules)
  treeDat <- strsplit(treeDat, "\n")[[1]]
  treeDat <- grep("att=", treeDat, value = TRUE)
  treeDat
  
}
getAtt <- function(x)
  {
    strt <- regexpr("att=", x)
    if(length(strt) == 0) stop("cannot parse model file")
    strt <- strt + 5
    stp <- regexpr("(forks=)|(cut=)|(val=)", x)
    if(length(stp) == 0) stop("cannot parse model file")
    stp <- stp -3
    substring(x, strt, stp)
  }

C5predictors <- function(x, ...) unique(getAtt(getVars(x)))


if(FALSE)
  {
    library(C5.0)

    test <- matrix(0, 3, 3)
    test[1, 1] <- 10
    test[1, 2] <- 1
    test[3, 2] <- 2
    mod <- C5.0(iris[, 1:4], iris$Species, costs = test)

    C5.0(iris[, 1:4], iris$Species, costs = test,
         trials = 100,
         control=C5.0Control(
           CF = .1, winnow = TRUE,
           sample = .10, bands = 10, fuzzyThreshold = TRUE))


  }


getBoostResults <- function(x)
  {
    output <-  strsplit(x, "\n")[[1]]
    ## what above when sampling is used
    srt <- grep("^Trial\t", output)
    stp <- grep("^boost\t", output)

    ## error check for srt, stp
    if(length(srt) == 0 | length(stp) == 0) return(NULL)
    if(any(stp - srt <= 0)) return(NULL)
    if(length(srt) != length(stp)) return(NULL)
    
    if(length(stp) > 1)
      {
        trainSrt <- grep("Evaluation on training data", output)
        testSrt <- grep("Evaluation on test data", output)
        if(testSrt < trainSrt)
          {
            srt <- rev(srt)
            stp <- rev(stp)
          }
        trainBoost <- parseBoostTable(output[(srt[1] + 4):(stp[1] - 1)])
        trainBoost$Data <- "Training Set"
        testBoost <- parseBoostTable(output[(srt[2] + 4):(stp[2] - 1)])
        testBoost$Data <- "Test Set"
        boostResults <- rbind(trainBoost, testBoost)        
      } else {
        boostResults <- parseBoostTable(output[(srt[1] + 4):(stp[1] - 1)])
        boostResults$Data <- "Training Set"
      }
    boostResults
    
    
  }

parseBoostTable <- function(x)
  {
    x <- gsub("(", " ", x, fixed = TRUE)
    x <- gsub("%)", "", x, fixed = TRUE)    
    x <- strsplit(x, "[[:space:]]")
    x <- lapply(x, function(x) x[x!= ""])

    if(all(unlist(lapply(x, length)) %in% 4:5))
      {
        x <- do.call("rbind", x)
        x <- matrix(as.numeric(x), ncol = ncol(x))
        x <- as.data.frame(x)
        cls <- c("Trial", "Size", "Errors", "Percent", "Cost")
        colnames(x) <- cls[1:ncol(x)]
        x$Trial <-  x$Trial + 1
      } else x <- NULL
    x
  }
