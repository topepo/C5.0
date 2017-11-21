makeDataFile <- function(x, y, w = NULL)
  {
    if(!is.data.frame(x)) x <- as.data.frame(x)
    convert <- unlist(lapply(x, function(x) is.factor(x) | is.character(x)))
    if(any(convert))
      {
        for(i in names(convert)[convert]) x[,i] <- formatCharacters(as.character(x[,i]))
      }
    if(is.null(y)) y <- rep(NA_real_, nrow(x))
    y <- formatCharacters(as.character(y))
    x <- cbind(y, x)
    if(!is.null(w)) x <- cbind(x, w)
    ## Determine the locations of missing values
    naIndex <- lapply(x, function(x) which(is.na(x)))
    anyNA <- any(unlist(lapply(naIndex, length)) > 0)
    x <- as.matrix(format(x, digits = 15, scientific = FALSE))
    ## remove leading white space
    x <- gsub("^[[:blank:]]*", "", x)
    ## reset missing values
    if(anyNA)
      {
        for(i in seq(along = naIndex)) if(length(naIndex[[i]]) > 0) x[naIndex[[i]],i] <- "?"
      }
    ## This line suggested by Barry Rowlingson on 04/21/12
    paste(apply(x, 1, paste, collapse=","), collapse = "\n")
  }

