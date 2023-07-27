# rib

[![CRAN status](https://www.r-pkg.org/badges/version-ago/rib?color=red)](https://CRAN.R-project.org/package=rib)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/rib)

**An R implementation of Interactive Brokers API**

Originally inspired by [`IBrokers`](https://CRAN.R-project.org/package=IBrokers),
`rib` is a native [R](https://www.r-project.org/) client that
implements the [Interactive Brokers](https://www.interactivebrokers.com/) API
to communicate with TWS or IBGateway.

It aims to be feature complete, however it does not support legacy versions.
Currently, only API versions `v176+` are supported.

The package design mirrors the official C++/Java
[IB API](https://interactivebrokers.github.io/tws-api/),
which is based on an asynchronous communication model over TCP.

### Installation
To install from [CRAN](https://CRAN.R-project.org/package=rib):
```R
install.packages("rib")
```

To install the latest snapshot from GitHub, assuming
[`devtools`](https://CRAN.R-project.org/package=devtools) or at least
[`remotes`](https://CRAN.R-project.org/package=remotes) is already installed:
```R
remotes::install_github("lbilli/rib")
```

### Usage
The user interacts mainly with two classes, implemented as
[`R6`](https://CRAN.R-project.org/package=R6) objects:
- `IBClient`: responsible to establish the connection and send requests to the server
- `IBWrap`: base class holding the callbacks that are executed when responses
  are processed. User customizations are derived from this class.

Other data structures, such as `Contract` and `Order`, are implemented as R lists,
or nested lists, and mirror the respective classes of the official IB API.

A complete minimal working example is shown.
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
    # Customized methods go here
    error=            function(id, errorCode, errorString, advancedOrderRejectJson)
                        cat("Error:", id, errorCode, errorString, advancedOrderRejectJson, "\n"),

    nextValidId=      function(orderId)
                        cat("Next OrderId:", orderId, "\n"),

    managedAccounts=  function(accountsList)
                        cat("Managed Accounts:", accountsList, "\n")

    # more method overrides can go here...
  )
)

# Instantiate wrapper and client
wrap <- IBWrapCustom$new()
ic   <- IBClient$new()

# Connect to the server with clientId = 1
ic$connect(port=4002, clientId=1)

# Check connection messages (optional)
ic$checkMsg(wrap)

# Define contract
contract <- IBContract(symbol="GOOG", secType="STK", exchange="SMART", currency="USD")

# Define order
order <- IBOrder(action="BUY", totalQuantity=10, orderType="LMT", lmtPrice=1000)

orderId <- 1    # Should match whatever is returned by the server

# Send order
ic$placeOrder(orderId, contract, order)

# Check inbound messages
ic$checkMsg(wrap)

# Disconnect
ic$disconnect()
```

As R is single threaded, in general it is the user's responsibility to code
some kind of event loop to periodically process incoming messages:
_i.e._ there is no background `Reader` monitoring the connection
and processing the server responses.

Two possible approaches, with or without a loop, are described:

##### Straight Request-Response pattern. No loop.
This is the simplest case. It's not suitable for data subscriptions or whenever a
stream of messages is expected. It follows the pattern:
- connect
- send requests
- process responses
- disconnect

```R
library(rib)

# Instantiate wrapper, client and connect
wrap <- IBWrapSimple$new()
ic   <- IBClient$new()
ic$connect(port=4002, clientId=1)

# Send requests, e.g.:
contract <- IBContract(symbol="GOOG", secType="STK", exchange="SMART", currency="USD")
ic$reqContractDetails(11, contract)

# more requests can go here...

# Parse responses
# Might need to be called several times to exhaust all messages
ic$checkMsg(wrap)

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
ic   <- IBClient$new()
ic$connect(port=4002, clientId=1)

# Main loop
repeat {

  # Application logic goes here
  # e.g.: send request
  ic$reqContractDetails(...)

  # Wait and process responses
  ic$checkMsg(wrap)

  if(done)
    break

  # Might be convenient to have a little wait here
  Sys.sleep(1)
}

# Disconnect
ic$disconnect()
```

### Implementation Details

##### [`IBClient`](R/IBClient.R)
This implements the IB `EClient` class functionality. Among its methods:
- `new()`: create a new instance.
- `connect(host, port, clientId, connectOptions)`: establish a connection.
- `disconnect()`: terminate the connection.
- `checkMsg(wrap, timeout)`: wait for responses and dispatch callbacks defined
  in `wrap`, which must be an instance of a class derived from `IBWrap`.
  If no response is available, it **blocks** up to `timeout` seconds.
  If `wrap` is missing, messages are taken off the wire and discarded.
  Return the number of responses processed. **Needs to be called regularly!**
- methods that send requests to the server.
  Refer to the official IB `EClient` class documentation for further details and
  method signatures.

##### [`IBWrap`](R/IBWrap.R)
Like the official IB `EWrapper` class, this holds the callbacks that are dispatched
when responses are processed. `IBWrap` itself contains only stubs.
Users need to derive from it and override the desired methods.
The code [above](#usage) provides a template of the procedure.

For a more extensive example refer to the definition of
[`IBWrapSimple`](examples/IBWrapSimple.R), mainly provided for
illustrative purposes, which simply prints out the content of the responses
or store it within `IBWrapSimple$context` for later inspection.

For more details about callback definitions and signatures,
refer to the official IB `EWrapper` class documentation.

### Notes
Callbacks are generally invoked with arguments and types matching the signatures
as described in the official documentation.
However, there are few exceptions:
- `tickPrice()` has an extra `size` argument,
  which is meaningful only when `TickType ∈ {BID, ASK, LAST}`.
  In these cases, the official IB API fires an extra `tickSize()` event instead.
- `historicalData()` is invoked only once per request,
  presenting all the historical data as a single `data.frame`,
  whereas the official IB API invokes it row-by-row.
- `scannerData()` is also invoked once per request and its arguments
  are in fact vectors rather than single values.

These modifications make it possible to establish the rule:
_one callback per server response_.

Consequently, `historicalDataEnd()` and `scannerDataEnd()` are redundant and
are **not** used in this package.

`data.frame` are passed to several other callbacks, such as:
`softDollarTiers()`, `familyCodes()`, `mktDepthExchanges()`,
`smartComponents()`, `newsProviders()`, `histogramData()`,
`marketRule()` and the `historicalTicks*()` family.

##### Missing Values
Occasionally, for numerical types, there is the need to represent
the lack of a value.

IB API does not have a uniform solution across the board, but rather
it adopts a variety of sentinel values.
They can be either the plain `0` or the largest representable value
of a given type such as `2147483647` and `9223372036854775807`
for 32- and 64-bit integers respectively or `1.7976931348623157E308`
for 64-bit floating point.

This package makes an effort to use R built-in `NA`
in all circumstances.

##### Data Structures
Other classes that mainly hold data are also [replicated](R/structs.R).
They are implemented as R lists, possibly nested, with names, types and default values
matching the IB API counterparts: _e.g._
`Contract`, `Order`, `ComboLeg`, `ExecutionFilter`, `ScannerSubscription`
and `Condition`.

To use them, simply make a copy and override their elements:
```R
contract          <- Contract
contract$conId    <- 12345
contract$currency <- "USD"
```
In the case of `Contract` and `Order`, helper [functions](R/factory.R)
are also provided to simplify the assignment to a subset of the fields:
```R
contract <- IBContract(symbol="GOOG", secType="STK", exchange="SMART", currency="USD")

# Equivalent to
contract <- Contract
contract$symbol   <- "GOOG"
contract$secType  <- "STK"
contract$exchange <- "SMART"
contract$currency <- "USD"
```
and
```R
order <- IBOrder(action="BUY", totalQuantity=100, orderType="LMT", lmtPrice=50)

# Equivalent to
order <- Order
order$action        <- "BUY"
order$totalQuantity <- 100
order$orderType     <- "LMT"
order$lmtPrice      <- 50
```
To instantiate a `Condition`, invoke `fCondition(type)`
and fill in the appropriate fields:
```R
condition <- fCondition("Price")
condition$conId    <- 265598L
condition$exchange <- "SMART"
condition$is_more  <- TRUE
condition$value    <- 200
```
`TagValueList` are implemented as R named character vectors.
Wherever a `TagValueList` is needed, something like this can be used:
```R
tagvaluelist <- c(tag1="value1", tag2="value2")
# or, in case of an empty list:
emptylist <- character()
```
