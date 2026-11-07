# Fit a model through the text interface that `C5.0()` used up to version
# 0.2.0: the data are written as `.names`/`.data` text and parsed by the C
# code. The `.C()` entry point is still in the package for predictions from
# old fitted objects, so the tests use it to check that both interfaces build
# the same model.
legacy_c50_fit <- function(
  x,
  y,
  weights = NULL,
  costs = NULL,
  trials = 1,
  rules = FALSE,
  control = C5.0Control(seed = 1)
) {
  names_string <- Cubist::makeNamesFile(
    x,
    y,
    w = weights,
    label = control$label,
    comments = FALSE
  )
  data_string <- Cubist::makeDataFile(x, y, weights)
  cost_string <- ""
  if (!is.null(costs)) {
    cost_string <- makeCostFile(costs)
  }
  .C(
    "C50",
    as.character(names_string),
    as.character(data_string),
    as.character(cost_string),
    as.logical(control$subset),
    as.logical(rules),
    as.integer(control$bands),
    as.integer(trials),
    as.logical(control$winnow),
    as.double(control$sample),
    as.integer(control$seed),
    as.integer(control$noGlobalPruning),
    as.double(control$CF),
    as.integer(control$minCases),
    as.logical(control$fuzzyThreshold),
    as.logical(control$earlyStopping),
    tree = character(1),
    rules = character(1),
    output = character(1),
    PACKAGE = "C50"
  )[c("tree", "rules", "output")]
}

# The model text starts with a line that has the date of the fit
strip_model_id <- function(x) {
  gsub("id=\"[^\"]*\"\n", "", x)
}
