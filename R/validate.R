#
# Create an environment containing functions that recursively convert list of parameters
#
generate_validator <- function()
{
  ev <- new.env()

  ev$i <- function(x) {
                        if(is.character(x))
                          x[x == "2147483647" | x == "9223372036854775807"] <- NA_character_

                        as.integer(x)
                      }

  ev$n <- function(x) {
                        if(is.character(x))
                          x[x == "1.7976931348623157E308"] <- NA_character_

                        as.numeric(x)
                      }

  ev$l <- function(x) {
                        if(is.character(x))
                          x == "true" | x != "0"
                        else
                          as.logical(x)
                      }

  # TODO remove this?
  #ev$c <- as.character

  ev$.validate <- function(arglist, names, types, no_names=FALSE) {

    stopifnot(is.list(arglist),
              length(names) == length(types))

    # Check names unless arglist is unnamed AND no_names is TRUE
    if(is.null(names(arglist)))
      stopifnot(no_names)
    else
      stopifnot(names(arglist) == names)

    # TODO and use this?
    for(i in which(types != "c"))
    #for(i in seq_along(types))
      arglist[[i]] <- ev[[types[i]]](arglist[[i]])

    arglist
  }


  #
  # Structs
  #
  for(i in seq_len(nrow(signature_struct)))

    ev[[signature_struct$struct[i]]] <- (function() {
                                            names <- strsplit(signature_struct$name[i], ",", fixed=TRUE)[[1]]
                                            types <- strsplit(signature_struct$type[i], ",", fixed=TRUE)[[1]]

                                            function(arglist) ev$.validate(arglist, names=names, types=types)
                                          }
                                        )()

  # Handle TagValue fields as named character vectors
  ev$TagValue <- function(arglist) {

    stopifnot(is.character(arglist),
              length(arglist) == 0L || !is.null(names(arglist)) && all(nzchar(names(arglist), keepNA=TRUE)))

    arglist
  }

#   # Handle TickAttrib as simple named bool vector
#   ev$TickAttrib <- function(arglist) {
#
#     stopifnot(is.logical(arglist),
#               names(arglist) == c("canAutoExecute", "pastLimit", "preOpen"))
#     arglist
#   }

  # Handle oDeltaNeutralContract as optional DeltaNeutralContract. NA when missing
  ev$oDeltaNeutralContract <-function(arglist) {

    if(is.list(arglist))
      ev$DeltaNeutralContract(arglist)

    else {
      stopifnot(is.na(arglist))

      NA
    }
  }


  ev$Condition <- (function() {

                      names <-
                      types <- list()

                      for(type in names(IBEnum$Condition)) {

                        condition <- fCondition(type)

                        names[[type]] <- names(condition)
                        types[[type]] <- substr(vapply(condition, class, NA_character_), 1, 1)
                      }

                      function(arglist) {

                        type <- arglist$type
                        stopifnot(type %in% names(names))

                        ev$.validate(arglist, names=names[[type]], types=types[[type]])
                      }
                    }
                  )()

  # Handle list of structs, possibly empty
  ev$.validate_vStruct <- function(arglist, struct) {

    stopifnot(is.list(arglist),
              is.null(names(arglist)))

    for(i in seq_along(arglist))
      arglist[[i]] <- ev[[struct]](arglist[[i]])

    arglist
  }

  for(s in c("ComboLeg",
             "ContractDescription",
             "ContractDetails",
             "Condition"))

    ev[[paste0("v", s)]] <- (function() {

                                 struct <- s

                                 function(arglist) ev$.validate_vStruct(arglist, struct=struct)
                              }
                            )()

  # dStruct
  for(s in c("Bar",
             "DepthMktDataDescription",
             "FamilyCode",
             "HistogramData",
             "HistoricalTick",
             "HistoricalTickBidAsk",
             "HistoricalTickLast",
             "NewsProvider",
             "PriceIncrement",
             "SmartComponent",
             "SoftDollarTier"))

    ev[[paste0("d", s)]] <- (function() {

                                struct <- s

                                function(arglist) {

                                  stopifnot(is.character(arglist),
                                            is.matrix(arglist))

                                  res <- as.data.frame(arglist, stringsAsFactors=FALSE)

                                  ev[[struct]](res)
                                }
                              })()

  #
  # Callback functions
  #

  # Skip functions with no parameters
  idx <- which(!is.na(signature_wrap$type))

  for(i in idx)

    ev[[signature_wrap$`function`[i]]] <- (function() {
                                            names <- strsplit(signature_wrap$args[i], ",", fixed=TRUE)[[1]]
                                            types <- strsplit(signature_wrap$type[i], ",", fixed=TRUE)[[1]]

                                            function(arglist, no_names=FALSE)
                                                    ev$.validate(arglist, names=names, types=types, no_names=no_names)
                                          }
                                        )()
  ev
}
