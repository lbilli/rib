#
# Helper functions to generate IB structs
#

setfields <- function(s, ...)
{
  args <- list(...)

  k <- names(args)

  stopifnot(!is.null(k))

  idx <- match(k, names(s))

  stopifnot(!is.na(idx))

  s[idx] <- args

  s
}

IBContract <- function(...) setfields(Contract, ...)

IBOrder <- function(...) setfields(Order, ...)

#
# Generate Condition structs
#
fCondition <- function(type)
{
  stopifnot(is.character(type))

  res <- switch(type,
                Price=         list(isMore= FALSE, price= 0, conId= 0L, exchange= "", triggerMethod= 0L),
                Time=          list(isMore= FALSE, time= ""),
                Margin=        list(isMore= FALSE, percent= 0L),
                Execution=     list(secType= "", exchange= "", symbol= ""),
                Volume=        list(isMore= FALSE, volume= 0L, conId= 0L, exchange= ""),
                PercentChange= list(isMore= FALSE, changePercent= NA_real_, conId= 0L, exchange= ""),
                stop("unknown Condition type"))

  c(type=type, conjunction="o", res)
}
