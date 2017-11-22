
makeCostFile <- function(cst) {
  classes <- colnames(cst)
  out <- ""
  for (i in 1:nrow(cst)) {
    for (j in 1:ncol(cst)) {
      if (i != j && cst[i, j] > 1) {
        out <- paste(out,
                     paste(classes[i], ", ", classes[j],
                           ": ", cst[i, j], "\n", sep = ""),
                     sep = "")
      }
    }
  }
  out
}
