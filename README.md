# rib

**An R implementation of Interactive Brokers API**

Originally inspired by [`IBrokers`](https://CRAN.R-project.org/package=IBrokers),
`rib` is a native [R](https://www.r-project.org/) client
implementing [Interactive Brokers](https://www.interactivebrokers.com/) API
to communicate with their TWS or IBGateway.

It aims to be feature complete, though it does not support legacy versions:
_i.e._ only API versions `v100` and above are currently supported.
This limit may become even stricter in the future.

The package design mirrors the official C++/Java
[IB API](http://interactivebrokers.github.io/tws-api/),
which is based on an asynchronous request-response communication model
over a TCP socket.

### Installation
To install from GitHub,
assuming [`devtools`](https://CRAN.R-project.org/package=devtools) or at least
[`remotes`](https://CRAN.R-project.org/package=remotes) is already installed:
```R
remotes::install_github("lbilli/rib")
```

### Usage
The user interacts mainly with two classes, implemented here as
[`R6`](https://CRAN.R-project.org/package=R6) objects:
- `IBClient`: responsible to establish the connection and send requests to the server.
- `IBWrap`: base class holding the callbacks that are executed when the
  client processes the responses. Customized versions are derived from this class.

Other data structures, such as `Contract` and `Order`, are implemented as R lists,
or nested lists, that mirror the respective classes in the official IB API.

A complete minimal working example follows.
For this code to work, an instance of the IB TWS or IBGateway needs to be running
on the local machine, listening on port `4002`.
**Note:** _demo_ or _paper_ account recommended!! :smirk:
```R
library(rib)

# Define a customized callbacks wrapper
IBWrapCustom <- R6::R6Class("IBWrapCustom",
  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  inherit= IBWrap,

  public= list(
    # Customized methods
    connectAck=       function() {},

    connectionClosed= function() cat("Connection Closed.\n"),

    error=            function(id, errorCode, errorString) cat(id, errorCode, errorString, "\n"),

    nextValidId=      function(orderId) cat("Next OrderId:", orderId, "\n"),

    managedAccounts=  function(accountsList) cat("Managed Accounts:", accountsList, "\n")
  )
)

# Instantiate wrapper and client
wrap <- IBWrapCustom$new()
ic   <- IBClient$new(wrap)

# Connect to the server with clientId = 1
ic$connect(port=4002, clientId=1)

# Check connection messages (optional)
ic$checkMsg()

# Define contract
contract <- IBContract("GOOG")

# Define order
order <- IBOrder("BUY", 10, "LMT", 1000)

orderId <- 1    # Should match whatever is returned by the server

# Send order
ic$placeOrder(orderId, contract, order)

# Check messages
ic$checkMsg()

# Disconnect
ic$disconnect()
```

As R is single threaded, in general it is the user's responsability to code
an event loop that handles the request-response pattern:
_i.e._ there is no `Reader` in the background monitoring the connection
and processing the server responses.

Two possible approaches, with or without a loop, are described:

##### Straight Request-Response pattern. No loop.
This is the simplest case. Not suitable for data subscriptions or whenever a
stream of messages are expected. It follows the pattern:
- connect
- send requests
- process responses
- disconnect

```R
library(rib)

# Instantiate wrapper, client and connect
wrap <- IBWrapSimple$new()
ic   <- IBClient$new(wrap)
ic$connect(port=4002, clientId=1)

# Send requests, e.g.:
contract <- IBContract("GOOG")
ic$reqContractDetails(11, contract)

# more requests can go here...

# Parse responses.
# Might need to be called several times to process all messages
ic$checkMsg(2)

# Find results in
wrap$context

# Disconnect
ic$disconnect()
```

##### Request-Response loop
A more realistic application requires a loop around the previous example:
- connect
- repeat
  - send requests
  - process responses
  - if done exit
- disconnect

```R
library(rib)

# Instantiate wrapper, client and connect
wrap <- IBWrapSimple$new()
ic   <- IBClient$new(wrap)
ic$connect(port=4002, clientId=1)

# Main loop
repeat {

  # Application logic goes here
  # e.g.: send request
  ic$reqContractDetails(...)

  # Wait and process responses
  ic$checkMsg(2)

  if(done)
    break

  # It is convenient to have a little wait here
  Sys.sleep(1)
}

# Disconnect
ic$disconnect()
```

### Implementation Details

##### [`IBClient`](R/IBClient.R)
This implements the IB `EClient` class functionality. Among its methods:
- `new(wrap)`: constructor. Require an object derived from `IBWrap` as argument.
- `replaceWrap(wrap)`: replace the `wrap`. As the client runs in a single thread,
  it is possible to replace the set of callbacks in a connected client.
- `connect(host, port, clientId, connectOptions)`: establish a connection.
- `disconnect()`: close the connection.
- `checkMsg(timeout)`: wait for responses and dispatch callbacks.
  If no response is available, it **blocks** up to `timeout` seconds.
  Return the number of responses processed. **Needs to be called regularly**.
- `flush()`: flush unprocessed responses, without dispatching callbacks.
  Mostly for debugging or testing.
- all other methods send specific requests to the server.
  Refer to the official IB `EClient` documentation for details and method signatures.

##### [`IBWrap`](R/IBWrap/R)
Like the official IB `EWrapper`, this holds the callbacks that are dispatched
when processing the responses. `IBWrap` itself is a base class containing
only dummy methods.
Users need to derive from it and customize the desired methods.

Refer to the code [above](#usage) or, for a more extensive example,
to the definition of [`IBWrapSimple`](R/IBWrapSimple.R), which is provided for
illustrative purposes and which prints out the content of the responses or store it
within `IBWrapSimple$context` for later inspection.

For more details regarding callback definitions and signatures,
refer again to the official IB `EWrapper` documentation.

##### Notes
Callbacks are generally invoked with arguments and types matching the signatures
found in the official documentation. However, there are few notable exceptions:
- `tickPrice()` has an extra `size` argument,
  which is meaningful only when `TickType = {BID, ASK, LAST}`.
  In these cases, the official IB API fires an extra `tickSize()` event instead.
- `historicalData()` is invoked only once per request,
  presenting all the historical data as a single `data.frame`,
  whereas the official IB API invokes it row-by-row.
- `scannerData()` is also invoked once per request and its arguments
  are in fact vectors rather than single values.

These modifications make it possible to establish the rule:
_one callback per server response_.

As a corollary, `historicalDataEnd()` and `scannerDataEnd()` become redundant and
thus are **not** used in this package.

`data.frame` structures are used liberally as arguments in several other callbacks,
such as: `softDollarTiers()`, `familyCodes()`,
`mktDepthExchanges()`, `smartComponents()`, `newsProviders()`, `histogramData()`,
`marketRule()` and the `historicalTicks*()` family.

##### Data Structures
Other classes that mainly hold data are also [defined](R/structs.R) in the package.
They are implemented as simple R (nested) lists with names, types and default values
matching the IB API counterparts.
Examples are: `Contract`, `Order`, `ComboLeg`, `ExecutionFilter`, `ScannerSubscription`
and `Condition`.

To use them, simply make a copy and override their elements:
```R
contract          <- Contract
contract$conId    <- 12345
contract$currency <- "USD"
```
In the case of `Contract` and `Order`, convenience [functions](R/factory.R)
are also provided to prefill some common fields:
```R
contract <- IBContract("GOOG")

# Equivalent to
contract <- Contract
contract$symbol   <- "GOOG"
contract$secType  <- "STK"
contract$exchange <- "SMART"
contract$currency <- "USD"
```
and
```R
order <- IBOrder(totalQuantity=100, lmtPrice=50)

# Equivalent to
order <- Order
order$action        <- "BUY"
order$totalQuantity <- 100
order$orderType     <- "LMT"
order$lmtPrice      <- 50
```
To instantiate a `Condition` type, invoke `fCondition(type)` like this:
```R
condition <- fCondition("Price")
```
`TagValue` types are implemented as R named character vectors:
```R
# Wherever a TagValue is needed, something like this can be used:
c(tag1="value1", tag2="value2")
```
