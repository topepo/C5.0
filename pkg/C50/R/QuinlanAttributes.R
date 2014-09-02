
QuinlanAttributes <- function (x, ...) UseMethod("QuinlanAttributes")
QuinlanAttributes.numeric <- function(x, ...) "continuous."
QuinlanAttributes.factor <- function(x, ...) QuinlanAttributes(levels(x))
QuinlanAttributes.character <- function(x, ...) paste(paste(unique(formatCharacters(x)), collapse = ","), ".", sep = "")
QuinlanAttributes.ordered <- function(x, ...) paste("[ordered]", paste(formatCharacters(levels(x)), collapse = ","), ".", sep = "")
QuinlanAttributes.matrix <- function(x, ...) apply(x, 2, QuinlanAttributes)
QuinlanAttributes.data.frame <- function(x, ...) unlist(lapply(x,  QuinlanAttributes))

