# Text version of the cost matrix, in the format of a C5.0 `.costs` file
# ("predicted class, true class: cost"). New fits pass the matrix to the C code
# directly, so this is kept as the `cost` element of fitted objects, where
# `print()` and `predict()` use it to tell whether costs are in effect. A cost
# of 1 is the default, so only other off-diagonal values are written. (Up to
# version 0.2.0, only values greater than 1 were written, and these were the
# only costs that C5.0 used.)
makeCostFile <- function(cst) {
  classes <- colnames(cst)
  out <- ""
  for (i in 1:nrow(cst)) {
    for (j in 1:ncol(cst)) {
      if (i != j && cst[i, j] != 1) {
        out <- paste(
          out,
          paste(classes[i], ", ", classes[j], ": ", cst[i, j], "\n", sep = ""),
          sep = ""
        )
      }
    }
  }
  out
}
