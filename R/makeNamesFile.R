
makeNamesFile <-
  function(x, y, w = NULL, label = "outcome", comments = TRUE){
    if(comments) {
        call <- match.call()
        out <- paste("| Generated using ", R.version.string, "\n",
                     "| on ", format(Sys.time(), "%a %b %d %H:%M:%S %Y"), "\n",
                     "| function call: ", paste(paste(deparse(call)), collapse = "\n|"),
                     sep = "")
      } else out <- ""

    if(is.numeric(y)) {
        outcomeInfo <- ": continuous."
      } else {
        lvls <- formatCharacters(levels(y))
        prefix <- if(is.ordered(y)) "[ordered] " else ""
        outcomeInfo <- paste(": ",
                             prefix,
                             paste(lvls, collapse = ","),
                             ".", sep = "")
      }

    out <- paste(out,
                 "\n", label, ".\n",
                 "\n", label, outcomeInfo,
                 sep = "")
    varData <- QuinlanAttributes(x)
    if(!is.null(w)) {
#       out <- paste(out, "\ncase weight := CaseWeights.", sep = "")
      varData <- c(varData, "case weight" = "continuous.")
    }
    varData <- paste(formatCharacters(names(varData)), ":", varData, sep = "", collapse = "\n")
    out <- paste(out, "\n", varData, "\n", sep = "")
    out


  }


# TODO: In C5.0.default, user can pass in a costs matrix where the column names
# and row names are not in the same order, which would probably break this code.
# Fix.  NC ,2012-07-11 
makeCostFile <- function(cst) {
    classes <- colnames(cst)  
    out <- ""
    for(i in 1:nrow(cst)) {
        for(j in 1:ncol(cst)) {
            if(i != j && cst[i,j] > 1) {
                out <- paste(out,
                             paste(classes[i], ", ", classes[j],
                                   ": ", cst[i,j], "\n", sep = ""),
                             sep = "")
              }
          }
      }
    out
  }
