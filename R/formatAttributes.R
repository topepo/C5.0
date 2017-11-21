## From http://www.rulequest.com/see5-unix.html:

## Names, labels, classes, and discrete values are represented by
## arbitrary strings of characters, with some fine print:
##  
##  - Tabs and spaces are permitted inside a name or value, but C5.0
##    collapses every sequence of these characters to a single space.
##
##  - Special characters (comma, colon, period, vertical bar '|') can
##    appear in names and values, but must be prefixed by the escape
##    character   '\'. For example, the name "Filch, Grabbit, and Co."
##    would be written as 'Filch\, Grabbit\, and Co\.'. (Colons in
##    times and periods in numbers do not need to be escaped.)

formatCharacters <- function(x)
  {
    ## Note that "useBytes=TRUE" is specified to avoid errors
    ## such as "input string 18 is invalid in this locale".

    ## for some reason, escaping : doesn't work...
    x <- gsub(":", ".", x, fixed = TRUE, useBytes=TRUE)
    ## gsub special chars with escapes
    gsub("([^[:alnum:]^[:space:]])", '\\\\\\1' , x, useBytes=TRUE)
    
  }

if(FALSE)
  {
    for(i in levels(ticdata$STYPE))
      {
        print(i)
        l1 <- c("level.a","level:a")
        set.seed(2)
        testData <- data.frame(class = sample(LETTERS[1:2], size = 20, replace = TRUE))
        testData$A = ifelse(testData$class == "A", l1[1], l1[2])
        testData$A[1:5] <- l1[1]
        testData$B <- rnorm(nrow(testData))
        names(testData)[2] <- "A"
        test1 <- C5.0(testData[,-1], testData[,1])
        summary(test1)
      }
  }

