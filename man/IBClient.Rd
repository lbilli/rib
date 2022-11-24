\name{IBClient}
\alias{IBClient}
\docType{class}

\title{Client Connection Class}

\description{
This is the main class that manages the connection with the
'Trader Workstation', sends requests and handles responses.
}

\section{Methods}{
\itemize{

\item \code{IBClient$new()}: creates a new instance.

\item \code{$connect(host="localhost", port, clientId, connectOptions="")}:
       connects to \code{host:port} and performs the initial
       handshake using client identifier \code{clientId} and additional
       options \code{connectOptions}.

\item \code{$checkMsg(wrap, timeout=0.2)}: waits for and process server messages.
      When available, messages are decoded and handed over to the appropriate
      callback defined in \code{wrap}, which must be an instance of a child of
      \code{IBWrap}.
      If \code{wrap} is missing, messages are read and immediately discarded.
      Returns the number of messages processed.

      This methods \bold{blocks} up to \code{timeout} seconds.
      \bold{Needs to be called regularly}.

\item \code{$disconnect()}: terminates the connection.
}

This class is modeled after the class \code{EClient} from the official IB API
implementations.
In addition to the methods shown above, several others exist that are used to send
requests to the server.

Refer to the official documentation for a comprehensive list of the possible
requests, including their signatures and descriptions.
}

\seealso{
\code{\link{IBWrap}}.

\href{https://interactivebrokers.github.io/tws-api/classIBApi_1_1EClient.html}{\code{EClient}}
definition from the official documentation.
}


\examples{
\dontrun{
# Instantiate a wrapper
wrap <- IBWrapSimple$new()

# Create a client and connect to a server
ic <- IBClient$new()
ic$connect(port=4002, clientId=1)

# Make a request
stock <- IBContract(symbol="GOOG", secType="STK", exchange="SMART", currency="USD")
ic$reqContractDetails(11, stock)

# Process responses
ic$checkMsg(wrap)

# Disconnect
ic$disconnect()
}
}
