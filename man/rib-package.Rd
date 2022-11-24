\name{rib-package}
\alias{rib-package}
\alias{rib}
\docType{package}

\title{An Implementation of 'Interactive Brokers' API}

\description{
\pkg{rib} allows programs to interact with 'Interactive Brokers' (IB)
'Trader Workstation' (TWS).
It establishes a connection over the network and manages
the exchange of messages between client and server.
Data is encoded and decoded between user and wire formats
following closely the official implementations.
}

\details{
The package design mirrors the implementations that
'Interactive Brokers' makes available for other languages,
such as C++, C#, Java and Python.

The official IB documentation is therefore the most valuable reference regarding
the API functionality and usage.
}

\seealso{
\url{https://github.com/lbilli/rib}

Report bugs at \url{https://github.com/lbilli/rib/issues}

The official
\href{https://interactivebrokers.github.io/tws-api/}{IB API documentation}.
}

\keyword{internal}
