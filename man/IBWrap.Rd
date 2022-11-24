\name{IBWrap}
\alias{IBWrap}
\docType{class}

\title{Callbacks Wrapper Class}

\description{
As the communication with the server is asynchronous,
the way to control how inbound messages are processed is via
callback functions.
The class \code{IBWrap} is merely a container for these functions.

Being a base class, its methods are just stubs whose only action is
to raise a warning when called.

Customized functionality is provided by defining a child class
of \code{IBWrap} and overriding the appropriate methods to perform
the desired tasks.

These methods are never called directly by the user program, rather
they are implicitly invoked within \code{IBClient$checkMsg()}
when it processes the server responses.
}

\details{
\code{IBWrap} is modeled after \code{EWrapper} from the official
IB API implementations.

The official documentation provides a comprehensive list and description
of the available methods, their signatures and usage.

The customization process follows this template:
\preformatted{
# Class derivation:
IBWrapSimple <- R6::R6Class("IBWrapSimple",
  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  inherit= IBWrap,

  public= list(

    # Customized methods:
    error=            function(id, errorCode, errorString, advancedOrderRejectJson){

                        # Code to handle error messages
                      },

    nextValidId=      function(orderId) {

                        # Code to handle the next order ID
                      },

    contractDetails=  function(reqId, contractDetails) {

                        # Code to handle Contract description
                      },
    # etc.
  )
)

# Class instantiation:
wrap <- IBWrapSimple$new()

# Use when processing server messages by a client:
ic <- IBClient$new()

ic$checkMsg(wrap)
}

}

\seealso{
\code{\link{IBClient}}.

\href{https://interactivebrokers.github.io/tws-api/interfaceIBApi_1_1EWrapper.html}{\code{EWrapper}}
definition from the official documentation.
}
