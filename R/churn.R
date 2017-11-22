

#' Customer Churn Data
#' 
#' A data set from the MLC++ machine learning software for modeling customer
#' churn. There are 19 predictors, mostly numeric: `state` (categorical),
#' `account_length` `area_code` `international_plan` (yes/no),
#' `voice_mail_plan` (yes/no), `number_vmail_messages`
#' `total_day_minutes` `total_day_calls` `total_day_charge`
#' `total_eve_minutes` `total_eve_calls` `total_eve_charge`
#' `total_night_minutes` `total_night_calls`
#' `total_night_charge` `total_intl_minutes`
#' `total_intl_calls` `total_intl_charge`, and
#' `number_customer_service_calls`.
#' 
#' The outcome is contained in a column called `churn` (also yes/no).
#' 
#' The training data has 3333 samples and the test set contains 1667.
#' 
#' A note in one of the source files states that the data are "artificial based
#' on claims similar to real world".
#' 
#' A rule-based model shown on the RuleQuest website contains 19 rules,
#' including: 
#' \preformatted{ 
#' Rule 1: (60, lift 6.8)
#'          international_plan = yes
#'          total_intl_calls <= 2
#'          ->  class yes  [0.984]
#' 
#' 
#' Rule 5: (43/2, lift 6.4)
#'         international_plan = no
#'         voice_mail_plan = no
#'         total_day_minutes > 246.6
#'         total_eve_charge > 20.5
#'         ->  class yes  [0.933]
#' 
#' Rule 10: (211/84, lift 4.1)
#'          total_day_minutes > 264.4
#'           ->  class yes  [0.601]
#' }
#' 
#' @name churn
#' @aliases churnTrain churnTest
#' @docType data
#' @return \item{churnTrain}{The training set} \item{churnTest}{The test set.}
#' @source \url{http://www.sgi.com/tech/mlc/},
#' \url{http://www.rulequest.com/see5-examples.html}
#' @keywords datasets
NULL



