\name{churn}
\docType{data}
\alias{churnTrain}
\alias{churnTest}
\title{Customer Churn Data}
\description{
A data set from the MLC++ machine learning software for modeling
customer churn. There are 19 predictors, mostly numeric: \code{state}
(categorical), \code{account_length}, \code{area_code},
\code{international_plan} (yes/no), \code{voice_mail_plan} (yes/no),
\code{number_vmail_messages}, \code{total_day_minutes},
\code{total_day_calls}, \code{total_day_charge},
\code{total_eve_minutes}, \code{total_eve_calls},
\code{total_eve_charge}, \code{total_night_minutes},
\code{total_night_calls}, \code{total_night_charge},
\code{total_intl_minutes}, \code{total_intl_calls},
\code{total_intl_charge} and \code{number_customer_service_calls}.

The outcome is contained in a column called \code{churn} (also yes/no).

The training data has 3333 samples and the test set contains 1667. 

A note in one of the source files states that the data are "artificial
based on claims similar to real world".

A rule-based model shown on the RuleQuest website contains 19 rules,
including:
\preformatted{
Rule 1: (2221/60, lift 1.1)
    international plan = no
    total day minutes <= 223.2
    number customer service calls <= 3
    ->  class 0  [0.973]

Rule 5: (1972/87, lift 1.1)
    total day minutes <= 264.4
    total intl minutes <= 13.1
    total intl calls > 2
    number customer service calls <= 3
    ->  class 0  [0.955]

Rule 10: (60, lift 6.8)
    international plan = yes
    total intl calls <= 2
    ->  class 1  [0.984]

Rule 12: (32, lift 6.7)
    total day minutes <= 120.5
    number customer service calls > 3
    ->  class 1  [0.971]
}

This implementation of C5.0 contains the same rules, but the rule numbers are different than above.
}

\usage{data(churn)}

\value{
  \item{churnTrain}{The training set}
  \item{churnTest}{The test set.}
  }

\source{\url{http://www.sgi.com/tech/mlc/}, \url{http://www.rulequest.com/see5-examples.html}
}

\keyword{datasets}  

