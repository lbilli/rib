\name{factory}
\title{Helpers}
\alias{IBContract}
\alias{IBOrder}
\alias{fCondition}

\description{
Helper functions that simplify the customization of common data structures.
}

\usage{
IBContract(\dots)

IBOrder(\dots)

fCondition(type)
}

\arguments{
  \item{\dots}{Any combination of named arguments whose names are valid for
               \code{Contract} or \code{Order} respectively.}

  \item{type}{Type of condition: one of \code{"Price"}, \code{"Time"},
              \code{"Margin"}, \code{"Execution"}, \code{"Volume"} or
              \code{"PercentChange"}.}
}

\details{
The same result is achieved by making a copy of the respective structures
and explicitly reassigning values to the desired fields.
The two approaches can be complementary.
}

\value{
\code{IBContract} returns a \code{Contract}.

\code{IBOrder} returns an \code{Order}.

\code{fCondition} returns a \code{Condition}.
}

\seealso{
\code{\link{Contract}}, \code{\link{Order}}.
}

\examples{
stock <- IBContract(symbol="GOOG", secType="STK", exchange="SMART", currency="USD")

# Equivalent to
stock <- Contract
stock$symbol   <- "GOOG"
stock$secType  <- "STK"
stock$exchange <- "SMART"
stock$currency <- "USD"

order <- IBOrder(action="BUY", totalQuantity=10, orderType="LMT", lmtPrice=99)

condition <- fCondition("Time")
condition$is_more <- TRUE
condition$value   <- "20221114-12:00"
}
