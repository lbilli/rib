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
  res <- switch(type,
                Price=         list(is_more= FALSE, value= 0, conId= 0L, exchange= "", triggerMethod= 0L),
                Time=          list(is_more= FALSE, value= ""),
                Margin=        list(is_more= FALSE, value= 0L),
                Execution=     list(secType= "", exchange= "", symbol= ""),
                Volume=        list(is_more= FALSE, value= 0L, conId= 0L, exchange= ""),
                PercentChange= list(is_more= FALSE, value= NA_real_, conId= 0L, exchange= ""),
                stop("unknown Condition type"))

  c(type=type, conjunction="o", res)
}
