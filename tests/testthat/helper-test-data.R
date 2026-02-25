# Helper functions for reproducible test data

make_two_class_data <- function(n = 100, seed = 4567) {
  set.seed(seed)
  data.frame(
    y = factor(sample(c("A", "B"), n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = factor(sample(letters[1:3], n, replace = TRUE))
  )
}

make_multi_class_data <- function(n = 100, n_classes = 3, seed = 3421) {
  set.seed(seed)
  classes <- LETTERS[1:n_classes]
  data.frame(
    y = factor(sample(classes, n, replace = TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
}

make_cost_matrix <- function(levels) {
  n <- length(levels)
  m <- matrix(1, n, n)
  m[1, 2] <- 2 # Cost of predicting class 2 when true class is 1
  dimnames(m) <- list(levels, levels)
  m
}
