#' @description
#'
#' Fit classification models using Quinlan's C5.0 algorithm, an extension of
#' C4.5. Models can be trees or rule sets, with optional boosting and
#' cost-sensitive learning. Supports factor, numeric, and ordered predictors,
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @useDynLib C50
#' @importFrom cli cli_abort cli_warn
#' @importFrom Cubist makeDataFile makeNamesFile QuinlanAttributes
#' @importFrom graphics plot
#' @importFrom partykit as.party as.partynode fitted_node party partynode partysplit
#' @importFrom stats .getXlevels as.formula delete.response model.extract
#' @importFrom stats model.frame model.response model.weights na.omit na.pass terms
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL
