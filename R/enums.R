#
# IB enums
#
IBEnum <- list(
               LegOpenClose= c(SAME=    0L,
                               OPEN=    1L,
                               CLOSE=   2L,
                               UNKNOWN= 3L),

               FaDataType= c(GROUPS=   1L,
                             ALIASES=  3L),

               MarketData= c(REALTIME=       1L,
                             FROZEN=         2L,
                             DELAYED=        3L,
                             DELAYED_FROZEN= 4L),

               Origin= c(CUSTOMER= 0L,
                         FIRM=     1L,
                         UNKNOWN=  2L),

               AuctionStrategy= c(UNSET=       0L,
                                  MATCH=       1L,
                                  IMPROVEMENT= 2L,
                                  TRANSPARENT= 3L),

               Condition= c(Price=         1L,
                            Time=          3L,
                            Margin=        4L,
                            Execution=     5L,
                            Volume=        6L,
                            PercentChange= 7L),

               PriceTrigger= c(Default=      0L,
                               DoubleBidAsk= 1L,
                               Last=         2L,
                               DoubleLast=   3L,
                               BidAsk=       4L,
                               LastBidAsk=   7L,
                               MidPoint=     8L)
               )

#
# Map IB enums <-> int
#
map_enum2int <- function(enum, name)
{
  stopifnot(is.character(enum),
            is.character(name))

  res <- IBEnum[[enum]]

  stopifnot(!is.null(res))

  res <- res[name]

  stopifnot(!is.na(res))

  res
}

map_int2enum <- function(enum, value)
{
  stopifnot(is.character(enum))

  res <- IBEnum[[enum]]

  stopifnot(!is.null(res))

  idx <- res == value

  stopifnot(any(idx))

  names(res)[idx]
}
