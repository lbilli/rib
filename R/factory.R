#
# Helper functions to generate IB structs
#

IBContract <- function(symbol, secType="STK", exchange="SMART", currency="USD")
{
  res <- Contract

  res$symbol   <- symbol
  res$secType  <- secType
  res$exchange <- exchange
  res$currency <- currency

  res
}

IBOrder <- function(action="BUY", totalQuantity, orderType="LMT", lmtPrice)
{
  res <- Order

  res$action        <- action
  res$totalQuantity <- totalQuantity
  res$orderType     <- orderType
  res$lmtPrice      <- lmtPrice

  res
}


#
# Generate Condition structs
#
fCondition <- function(type)
{
  res <- switch(type,
                Price=         list(is_more= FALSE, value= 0, conId= 0L, exchange= "", triggerMethod= 0L),
                Time=          list(is_more= FALSE, value= ""),
                Margin=        list(is_more= FALSE, value= 0L),
                Execution=     list(secType= "", exchange= "", symbol= ""),
                Volume=        list(is_more= FALSE, value= 0L, conId= 0L, exchange= ""),
                PercentChange= list(is_more= FALSE, value= NA_real_, conId= 0L, exchange= ""),
                stop("Unknown Condition type."))

  c(type=type, conjunction="o", res)
}
