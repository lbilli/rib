\name{structs}
\title{Data Structures}
\alias{ComboLeg}
\alias{Contract}
\alias{DeltaNeutralContract}
\alias{ExecutionFilter}
\alias{Order}
\alias{ScannerSubscription}
\alias{SoftDollarTier}
\alias{WshEventData}
\docType{data}

\description{
The data structures used by the API are implemented as \R named lists,
possibly nested.
Templates filled with default values are defined within the package.
In order to instantiate them, no elaborate contructor is required but
a simple copy will do.

Still, \link[=IBContract]{helper functions} are available for \code{Contract} and \code{Order}.
}

\usage{
ComboLeg

Contract

DeltaNeutralContract

ExecutionFilter

Order

ScannerSubscription

SoftDollarTier

WshEventData
}

\seealso{
\code{\link{IBContract}}, \code{\link{IBOrder}}.
}

\examples{
stock <- Contract

stock$symbol   <- "GOOG"
stock$secType  <- "STK"
stock$exchange <- "SMART"
stock$currency <- "USD"
}
