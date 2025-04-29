#
# Decode message
#
decode <- function(msg, ver)
{
  stopifnot(is.list(msg))

  msgid <- msg$msgid
  imsg <- msg$msg

  # The second field is unused version, for msgId < 75 and != 3, 4, 5, 10, 11, 17, 18, 21
  if(msgid < 75L && ! msgid %in% c(3L, 4L, 5L, 10L, 11L, 17L, 18L, 21L) ||
      msgid == 4L && ver < MIN_SERVER_VER_ERROR_TIME)
    imsg$pop()

  # Find handler
  handler <- process[[as.character(msgid)]]

  res <- if(is.null(handler)) {
            warning("unknown message id: ", msgid)
            NULL
          }
          else
          # Call the appropriate handler
          handler(imsg, ver)

  # Check that the full message has been processed
  if(msgid < PROTOBUF_MSG_ID && imsg$left() > 0L)
    warning("message: ", msgid, " not completely processed. Ignored: ",
            paste0(dQuote(imsg$pop(imsg$left())), collapse=" "))

  res
}

#
# Validate/convert args
#
validate <- function(fname, ..., no_names=FALSE)
{
  args <- list(...)

  # Handle parameters passed in a single character vector
  if(length(args) == 1L && is.character(args[[1L]]) && length(args[[1L]]) > 1L && !is.matrix(args[[1L]]))
    args <- as.list(args[[1L]])

  if(length(args) > 0L)
    args <- Validator[[fname]](args, no_names=no_names)

  # Return callback name and argument list
  list(fname=fname, fargs=args)
}

#
# Make a matrix by row
#
to_matrix <- function(imsg, struct, names)
{
  n <- Validator$i(imsg$pop())

  if(!missing(struct))
    names <- get("names", environment(Validator[[struct]]))

  matrix(imsg$pop(n * length(names)),
          ncol=     length(names),
          byrow=    TRUE,
          dimnames= list(character(), names))
}

#
# Make a list of structs
#
to_list <- function(imsg, struct)
{
  n <- Validator$i(imsg$pop())

  names <- get("names", environment(Validator[[struct]]))

  m <- length(names)

  lapply(seq_len(n), function(i) as.list(structure(imsg$pop(m), names=names)) )
}

#
# Unmask "mask" into a list of logicals
#
unmask <- function(mask, names)
{
  as.list(structure(intToBits(mask)[seq_along(names)] != 0L,
                    names=names))
}

# ##############################################################
#
# Message handlers
#
# ##############################################################
process <- list2env(list(

  # TICK_PRICE
  "1"= function(imsg, ver) {

    m <- imsg$pop(5L)

    # Convert tickType to string
    m[2L] <- ticktype(m[2L])

    attrib <- unmask(m[5L], c("canAutoExecute", "pastLimit", "preOpen"))

    validate("tickPrice", tickerId= m[1L],
                          field=    m[2L],
                          price=    m[3L],
                          size=     m[4L],
                          attrib=   attrib)
  },

  # TICK_SIZE
  "2"= function(imsg, ver) {

    m <- imsg$pop(3L)

    # Convert tickType to string
    m[2L] <- ticktype(m[2L])

    validate("tickSize", m, no_names=TRUE)
  },

  # TICK_OPTION_COMPUTATION
  "21"= function(imsg, ver) {

    m <- imsg$pop(11L)

    # Convert tickType to string
    m[2L] <- ticktype(m[2L])

    # (impliedVol, optPrice, pvDividend, undPrice) == -1 means NA
    idx <- c(4L, 6L, 7L, 11L)
    m[idx][m[idx] == "-1"] <- NA_character_

    # (delta, gamma, vega, theta) == -2 means NA
    idx <- c(5L, 8L, 9L, 10L)
    m[idx][m[idx] == "-2"] <- NA_character_

    validate("tickOptionComputation", m, no_names=TRUE)
  },

  # TICK_GENERIC
  "45"= function(imsg, ver) {

    m <- imsg$pop(3L)

    # Convert tickType to string
    m[2L] <- ticktype(m[2L])

    validate("tickGeneric", m, no_names=TRUE)
  },

  # TICK_STRING
  "46"= function(imsg, ver) {

    m <- imsg$pop(3L)

    # Convert tickType to string
    m[2L] <- ticktype(m[2L])

    validate("tickString", m, no_names=TRUE)
  },

  # TICK_EFP
  "47"= function(imsg, ver) {

    m <- imsg$pop(9L)

    # Convert tickType to string
    m[2L] <- ticktype(m[2L])

    validate("tickEFP", m, no_names=TRUE)
  },

  # ORDER_STATUS
  "3"= function(imsg, ver) {

    validate("orderStatus", imsg$pop(11L), no_names=TRUE)
  },

  # ERR_MSG
  "4"= function(imsg, ver) {

    m <- imsg$pop(4L)

    errorTime <- if(ver >= MIN_SERVER_VER_ERROR_TIME) imsg$pop() else "0"

    validate("error", id=          m[1L],
                      errorTime=   errorTime,
                      errorCode=   m[2L],
                      errorString= m[3L],
                      advancedOrderRejectJson= m[4L])
  },

  # OPEN_ORDER
  "5"= function(imsg, ver) {

    contract   <- Contract
    order      <- Order
    orderState <- OrderState

    order$orderId <- imsg$pop()

    contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

    order[c(4L:9L)] <- imsg$pop(6L) # "action" -> "tif"

    order[c("ocaGroup",
            "account",
            "openClose")] <- imsg$pop(3L)

    order$origin <- map_int2enum("Origin", imsg$pop())

    order[c("orderRef",
            "clientId",
            "permId",
            "outsideRth",
            "hidden",
            "discretionaryAmt",
            "goodAfterTime")] <- imsg$pop(7L)

    imsg$pop() # Deprecated sharesAllocation

    order[c("faGroup",
            "faMethod",
            "faPercentage",
            "modelCode",
            "goodTillDate",
            "rule80A",
            "percentOffset",
            "settlingFirm",
            "shortSaleSlot",
            "designatedLocation",
            "exemptCode")] <- imsg$pop(11L)

    order$auctionStrategy <- map_int2enum("AuctionStrategy", imsg$pop())

    order[43L:47L] <- imsg$pop(5L) # "startingPrice" -> "stockRangeUpper"

    order[c("displaySize",
            "blockOrder",
            "sweepToFill",
            "allOrNone",
            "minQty",
            "ocaType")] <- imsg$pop(6L)

    imsg$pop(3L) # Deprecated eTradeOnly, firmQuoteOnly, nbboPriceCap

    order[c("parentId",
            "triggerMethod")] <- imsg$pop(2L)

    order[50L:53L] <- imsg$pop(4L) # "volatility" -> "deltaNeutralAuxPrice"

    if(nzchar(order$deltaNeutralOrderType))
      order[54L:61L] <- imsg$pop(8L) # "deltaNeutralConId" -> "deltaNeutralDesignatedLocation"

    order[c("continuousUpdate",
            "referencePriceType",
            "trailStopPrice",
            "trailingPercent",
            "basisPoints",
            "basisPointsType")] <- imsg$pop(6L)

    contract$comboLegsDescrip <- imsg$pop()

    # ComboLegs
    contract$comboLegs <- to_list(imsg, "ComboLeg")

    # OrderComboLeg
    orderComboLegsCount <- Validator$i(imsg$pop())

    if(orderComboLegsCount > 0L)
      order$orderComboLegs <- imsg$pop(orderComboLegsCount)

    # SmartComboRouting
    smartComboRoutingParamsCount <- Validator$i(imsg$pop())

    if(smartComboRoutingParamsCount > 0L)
      order$smartComboRoutingParams <- fold_tagvalue(imsg$pop(2L * smartComboRoutingParamsCount))

    order[c("scaleInitLevelSize",
            "scaleSubsLevelSize")] <- imsg$pop(2L)

    order$scalePriceIncrement <- Validator$n(imsg$pop())

    if(!is.na(order$scalePriceIncrement) && order$scalePriceIncrement > 0L)
      order[69L:75L] <- imsg$pop(7L) # "scalePriceAdjustValue" -> "scaleRandomPercent"

    order$hedgeType <- imsg$pop()

    if(nzchar(order$hedgeType))
      order$hedgeParam <- imsg$pop()

    order[c("optOutSmartRouting",
            "clearingAccount",
            "clearingIntent",
            "notHeld")] <- imsg$pop(4L)

    # DeltaNeutralContract
    if(Validator$l(imsg$pop())) {

      contract$deltaNeutralContract <- DeltaNeutralContract

      contract$deltaNeutralContract[1L:3L] <- imsg$pop(3L)
    }

    # AlgoStrategy
    order$algoStrategy <- imsg$pop()

    if(nzchar(order$algoStrategy)) {

      algoParamsCount <- Validator$i(imsg$pop())

      if(algoParamsCount > 0L)
        order$algoParams <- fold_tagvalue(imsg$pop(2L * algoParamsCount))
    }

    order[c("solicited",
            "whatIf")] <- imsg$pop(2L)

    orderState[1L:14L] <- imsg$pop(14L) # "status" -> "commissionCurrency"

    if(ver >= MIN_SERVER_VER_FULL_ORDER_PREVIEW_FIELDS) {

      orderState[15L:26L] <- imsg$pop(12L) # "marginCurrency" -> "rejectReason"

      orderState$orderAllocations <- to_list(imsg, "OrderAllocation")
    }

    orderState$warningText <- imsg$pop()

    order[c("randomizeSize",
            "randomizePrice")] <- imsg$pop(2L)

    if(order$orderType == "PEG BENCH")
      order[c("referenceContractId",
              "isPeggedChangeAmountDecrease",
              "peggedChangeAmount",
              "referenceChangeAmount",
              "referenceExchangeId")] <- imsg$pop(5L)

    conditionsSize <- Validator$i(imsg$pop())

    if(conditionsSize > 0L) {

      order$conditions <- lapply(seq_len(conditionsSize),
                                  function(i) {

                                    cond <- fCondition(map_int2enum("Condition",
                                                                    Validator$i(imsg$pop())))

                                    cond[-1L] <- imsg$pop(length(cond) - 1L)

                                    cond
                                  })

      order[c("conditionsIgnoreRth",
              "conditionsCancelOrder")] <- imsg$pop(2L)
    }

    order[c("adjustedOrderType",
            "triggerPrice",
            "trailStopPrice",
            "lmtPriceOffset",
            "adjustedStopPrice",
            "adjustedStopLimitPrice",
            "adjustedTrailingAmount",
            "adjustableTrailingUnit")] <- imsg$pop(8L)

    order$softDollarTier[1L:3L] <- imsg$pop(3L)

    order[c("cashQty",
            "dontUseAutoPriceForHedge",
            "isOmsContainer",
            "discretionaryUpToLimitPrice",
            "usePriceMgmtAlgo",
            "duration",
            "postToAts",
            "autoCancelParent",
            "minTradeQty",
            "minCompeteSize",
            "competeAgainstBestOffset",
            "midOffsetAtWhole",
            "midOffsetAtHalf",
            "customerAccount",
            "professionalCustomer",
            "bondAccruedInterest")] <- imsg$pop(16L)

    if(ver >= MIN_SERVER_VER_INCLUDE_OVERNIGHT)
      order$includeOvernight <- imsg$pop()

    if(ver >= MIN_SERVER_VER_CME_TAGGING_FIELDS_IN_OPEN_ORDER)
      order[c("extOperator",
              "manualOrderIndicator")] <- imsg$pop(2L)

    if(ver >= MIN_SERVER_VER_SUBMITTER)
      order$submitter <- imsg$pop()

    if(ver >= MIN_SERVER_VER_IMBALANCE_ONLY)
      order$imbalanceOnly <- imsg$pop()

    validate("openOrder", orderId=    order$orderId,
                          contract=   contract,
                          order=      order,
                          orderstate= orderState)
  },

  # ACCT_VALUE
  "6"= function(imsg, ver) {

    validate("updateAccountValue", imsg$pop(4L), no_names=TRUE)
  },

  # PORTFOLIO_VALUE
  "7"= function(imsg, ver) {

    contract <- Contract

    contract[c(1L:7L, 9L:12L)] <- imsg$pop(11L)
    position       <- imsg$pop()
    marketPrice    <- imsg$pop()
    marketValue    <- imsg$pop()
    averageCost    <- imsg$pop()
    unrealizedPNL  <- imsg$pop()
    realizedPNL    <- imsg$pop()
    accountName    <- imsg$pop()

    validate("updatePortfolio", contract=      contract,
                                position=      position,
                                marketPrice=   marketPrice,
                                marketValue=   marketValue,
                                averageCost=   averageCost,
                                unrealizedPNL= unrealizedPNL,
                                realizedPNL=   realizedPNL,
                                accountName=   accountName)
  },

  # ACCT_UPDATE_TIME
  "8"= function(imsg, ver) {

    validate("updateAccountTime", timeStamp=imsg$pop())
  },

  # NEXT_VALID_ID
  "9"= function(imsg, ver) {

    validate("nextValidId", orderId=imsg$pop())
  },

  # CONTRACT_DATA
  "10"= function(imsg, ver) {

    reqId <- imsg$pop()

    cd <- ContractDetails

    cd$contract[c(2L, 3L, 4L, 18L, 5L, 6L, 8L, 10L, 11L)] <- imsg$pop(9L)

    cd$marketName               <- imsg$pop()
    cd$contract$tradingClass    <- imsg$pop()
    cd$contract$conId           <- imsg$pop()
    cd$minTick                  <- imsg$pop()
    cd$contract$multiplier      <- imsg$pop()
    cd[4L:8L]                   <- imsg$pop(5L)
    cd$contract$primaryExchange <- imsg$pop()
    cd[9L:17L]                  <- imsg$pop(9L)

    n <- Validator$i(imsg$pop())

    if(n > 0L)
      cd$secIdList <- fold_tagvalue(imsg$pop(2L * n))

    cd[c("aggGroup",
          "underSymbol",
          "underSecType",
          "marketRuleIds",
          "realExpirationDate",
          "stockType",
          "minSize",
          "sizeIncrement",
          "suggestedSizeIncrement")] <- imsg$pop(9L)

    if(cd$contract$secType == "FUND") {

      cd[44L:58L] <- imsg$pop(15L)

      cd$fundDistributionPolicyIndicator <- funddist(imsg$pop())
      cd$fundAssetType <- fundtype(imsg$pop())
    }

    cd$ineligibilityReasonList <- to_list(imsg, "IneligibilityReason")

    validate("contractDetails", reqId=reqId, contractDetails=cd)
  },

  # BOND_CONTRACT_DATA
  "18"= function(imsg, ver) {

    reqId <- imsg$pop()

    cd <- ContractDetails

    cd$contract[2L:3L] <- imsg$pop(2L)

    cd[c("cusip",
          "coupon",
          "maturity",
          "issueDate",
          "ratings",
          "bondType",
          "couponType",
          "convertible",
          "callable",
          "putable",
          "descAppend")] <- imsg$pop(11L)

    cd$contract[c("exchange",
                  "currency")] <- imsg$pop(2L)

    cd$marketName <- imsg$pop()

    cd$contract[c("tradingClass",
                  "conId")] <- imsg$pop(2L)

    cd[c("minTick",
          "orderTypes",
          "validExchanges",
          "nextOptionDate",
          "nextOptionType",
          "nextOptionPartial",
          "notes",
          "longName")] <- imsg$pop(8L)

    if(ver >= MIN_SERVER_VER_BOND_TRADING_HOURS)
      cd[13L:15L] <- imsg$pop(3L) # "timeZoneId" -> "liquidHours"

    cd[16L:17L] <- imsg$pop(2L) # "evRule" -> "evMultiplier"

    n <- Validator$i(imsg$pop())

    if(n > 0L)
      cd$secIdList <- fold_tagvalue(imsg$pop(2L * n))

    cd[c("aggGroup",
          "marketRuleIds",
          "minSize",
          "sizeIncrement",
          "suggestedSizeIncrement")] <- imsg$pop(5L)

    validate("bondContractDetails", reqId=reqId, contractDetails=cd)
  },

  # EXECUTION_DATA
  "11"= function(imsg, ver) {

    contract  <- Contract
    execution <- Execution

    reqId <- imsg$pop()

    execution$orderId <- imsg$pop()

    contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

    execution[2L:19L] <- imsg$pop(18L)

    if(ver >= MIN_SERVER_VER_SUBMITTER)
      execution$submitter <- imsg$pop()

    validate("execDetails", reqId=reqId, contract=contract, execution=execution)
  },

  # MARKET_DEPTH
  "12"= function(imsg, ver) {

    validate("updateMktDepth", imsg$pop(6L), no_names=TRUE)
  },

  # MARKET_DEPTH_L2
  "13"= function(imsg, ver) {

    validate("updateMktDepthL2", imsg$pop(8L), no_names=TRUE)
  },

  # NEWS_BULLETINS
  "14"= function(imsg, ver) {

    validate("updateNewsBulletin", imsg$pop(4L), no_names=TRUE)
  },

  # MANAGED_ACCTS
  "15"= function(imsg, ver) {

    validate("managedAccounts", accountsList=imsg$pop())
  },

  # RECEIVE_FA
  "16"= function(imsg, ver) {

    faDataType <- map_int2enum("FaDataType", imsg$pop())

    validate("receiveFA", faDataType=faDataType, xml=imsg$pop())
  },

  # HISTORICAL_DATA
  "17"= function(imsg, ver) {

    reqId <- imsg$pop()

    if(ver < MIN_SERVER_VER_HISTORICAL_DATA_END)
      imsg$pop(2L) # Ignore startDate, endDate

    bar <- to_matrix(imsg, "Bar")

    validate("historicalData", reqId=reqId, bar=bar)
  },

  # SCANNER_DATA
  "20"= function(imsg, ver) {

    reqId <- imsg$pop()

    n <- Validator$i(imsg$pop())

    cd <- vector(n, mode="list")

    rank       <-
    distance   <-
    benchmark  <-
    projection <-
    legsStr    <- character(n)

    for(i in seq_len(n)) {

      rank[i] <- imsg$pop()

      d <- ContractDetails
      d$contract[c(1L:6L, 8L, 10L, 11L)] <- imsg$pop(9L)
      d$marketName                       <- imsg$pop()
      d$contract$tradingClass            <- imsg$pop()

      cd[[i]] <- d

      distance[i]   <- imsg$pop()
      benchmark[i]  <- imsg$pop()
      projection[i] <- imsg$pop()
      legsStr[i]    <- imsg$pop()
    }

    validate("scannerData", reqId=           reqId,
                            rank=            rank,
                            contractDetails= cd,
                            distance=        distance,
                            benchmark=       benchmark,
                            projection=      projection,
                            legsStr=         legsStr)
  },

  # SCANNER_PARAMETERS
  "19"= function(imsg, ver) {

    validate("scannerParameters", xml=imsg$pop())
  },

  # CURRENT_TIME
  "49"= function(imsg, ver) {

    validate("currentTime", time=imsg$pop())
  },

  # REAL_TIME_BARS
  "50"= function(imsg, ver) {

    validate("realtimeBar", imsg$pop(9L), no_names=TRUE)
  },

  # FUNDAMENTAL_DATA
  "51"= function(imsg, ver) {

    reqId <- imsg$pop()

    validate("fundamentalData", reqId=reqId, data=imsg$pop())
  },

  # CONTRACT_DATA_END
  "52"= function(imsg, ver) {

    validate("contractDetailsEnd", reqId=imsg$pop())
  },

  # OPEN_ORDER_END
  "53"= function(imsg, ver) {

    validate("openOrderEnd")
  },

  # ACCT_DOWNLOAD_END
  "54"= function(imsg, ver) {

    validate("accountDownloadEnd", accountName=imsg$pop())
  },

  # EXECUTION_DATA_END
  "55"= function(imsg, ver) {

    validate("execDetailsEnd", reqId=imsg$pop())
  },

  # DELTA_NEUTRAL_VALIDATION
  "56"= function(imsg, ver) {

    reqId <- imsg$pop()

    deltaNeutralContract <- DeltaNeutralContract

    deltaNeutralContract[1L:3L] <- imsg$pop(3L)

    validate("deltaNeutralValidation", reqId=reqId, deltaNeutralContract=deltaNeutralContract)
  },

  # TICK_SNAPSHOT_END
  "57"= function(imsg, ver) {

    validate("tickSnapshotEnd", reqId=imsg$pop())
  },

  # MARKET_DATA_TYPE
  "58"= function(imsg, ver) {

    reqId <- imsg$pop()

    validate("marketDataType", reqId=reqId, marketDataType=imsg$pop())
  },

  # COMMISSION_REPORT
  "59"= function(imsg, ver) {

    commission <- CommissionReport

    commission[1L:6L] <- imsg$pop(6L)

    validate("commissionReport", commissionReport=commission)
  },

  # POSITION_DATA
  "61"= function(imsg, ver) {

    account <- imsg$pop()

    contract <- Contract
    contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

    position <- imsg$pop()
    avgCost  <- imsg$pop()

    validate("position", account=  account,
                         contract= contract,
                         position= position,
                         avgCost=  avgCost)
  },

  # POSITION_END
  "62"= function(imsg, ver) {

    validate("positionEnd")
  },

  # ACCOUNT_SUMMARY
  "63"= function(imsg, ver) {

    validate("accountSummary", imsg$pop(5L), no_names=TRUE)
  },

  # ACCOUNT_SUMMARY_END
  "64"= function(imsg, ver) {

    validate("accountSummaryEnd", reqId=imsg$pop())
  },

  # VERIFY_MESSAGE_API
  "65"= function(imsg, ver) {

    validate("verifyMessageAPI", apiData=imsg$pop())
  },

  # VERIFY_COMPLETED
  "66"= function(imsg, ver) {

    isSuccessful <- imsg$pop()
    validate("verifyCompleted", isSuccessful=isSuccessful, errorText=imsg$pop())
  },

  # DISPLAY_GROUP_LIST
  "67"= function(imsg, ver) {

    reqId <- imsg$pop()

    validate("displayGroupList", reqId=reqId, groups=imsg$pop())
  },

  # DISPLAY_GROUP_UPDATED
  "68"= function(imsg, ver) {

    reqId <- imsg$pop()

    validate("displayGroupUpdated", reqId=reqId, contractInfo=imsg$pop())
  },

  # VERIFY_AND_AUTH_MESSAGE_API
  "69"= function(imsg, ver) {

    apiData <- imsg$pop()

    validate("verifyAndAuthMessageAPI", apiData=apiData, xyzChallenge=imsg$pop())
  },

  # VERIFY_AND_AUTH_COMPLETED
  "70"= function(imsg, ver) {

    isSuccessful <- imsg$pop()

    validate("verifyAndAuthCompleted", isSuccessful=isSuccessful, errorText=imsg$pop())
  },

  # POSITION_MULTI
  "71"= function(imsg, ver) {

    reqId   <- imsg$pop()
    account <- imsg$pop()

    contract <- Contract
    contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

    position  <- imsg$pop()
    avgCost   <- imsg$pop()
    modelCode <- imsg$pop()

    validate("positionMulti", reqId=     reqId,
                              account=   account,
                              modelCode= modelCode,
                              contract=  contract,
                              position=  position,
                              avgCost=   avgCost)
  },

  # POSITION_MULTI_END
  "72"= function(imsg, ver) {

    validate("positionMultiEnd", reqId=imsg$pop())
  },

  # ACCOUNT_UPDATE_MULTI
  "73"= function(imsg, ver) {

    validate("accountUpdateMulti", imsg$pop(6L), no_names=TRUE)
  },

  # ACCOUNT_UPDATE_MULTI_END
  "74"= function(imsg, ver) {

    validate("accountUpdateMultiEnd", reqId=imsg$pop())
  },

  # SECURITY_DEFINITION_OPTION_PARAMETER
  "75"= function(imsg, ver) {

    reqId           <- imsg$pop()
    exchange        <- imsg$pop()
    underlyingConId <- imsg$pop()
    tradingClass    <- imsg$pop()
    multiplier      <- imsg$pop()

    nExp <- Validator$i(imsg$pop())
    expirations <- imsg$pop(nExp)

    nStrk <- Validator$i(imsg$pop())
    strikes <- imsg$pop(nStrk)

    validate("securityDefinitionOptionalParameter", reqId=           reqId,
                                                    exchange=        exchange,
                                                    underlyingConId= underlyingConId,
                                                    tradingClass=    tradingClass,
                                                    multiplier=      multiplier,
                                                    expirations=     expirations,
                                                    strikes=         strikes)
  },

  # SECURITY_DEFINITION_OPTION_PARAMETER_END
  "76"= function(imsg, ver) {

    validate("securityDefinitionOptionalParameterEnd", reqId=imsg$pop())
  },

  # SOFT_DOLLAR_TIERS
  "77"= function(imsg, ver) {

    reqId <- imsg$pop()

    tiers <- to_matrix(imsg, "SoftDollarTier")

    validate("softDollarTiers", reqId=reqId, tiers=tiers)
  },

  # FAMILY_CODES
  "78"= function(imsg, ver) {

    familyCodes <- to_matrix(imsg, "FamilyCode")

    validate("familyCodes", familyCodes=familyCodes)
  },

  # SYMBOL_SAMPLES
  "79"= function(imsg, ver) {

    reqId <- imsg$pop()

    cds <- lapply(seq_len(Validator$i(imsg$pop())), function(i) {

                            cd <- ContractDescription

                            cd$contract[c("conId",
                                          "symbol",
                                          "secType",
                                          "primaryExchange",
                                          "currency")] <- imsg$pop(5L)

                            nsec <- Validator$i(imsg$pop())

                            if(nsec > 0L)
                              cd$derivativeSecTypes <- imsg$pop(nsec)

                            cd$contract[c("description",
                                          "issuerId")] <- imsg$pop(2L)
                            cd
                          })

    validate("symbolSamples", reqId=reqId, contractDescriptions=cds)
  },

  # MKT_DEPTH_EXCHANGES
  "80"= function(imsg, ver) {

    dms <- to_matrix(imsg, "DepthMktDataDescription")

    validate("mktDepthExchanges", depthMktDataDescriptions=dms)
  },

  # TICK_REQ_PARAMS
  "81"= function(imsg, ver) {

    validate("tickReqParams", imsg$pop(4L), no_names=TRUE)
  },

  # SMART_COMPONENTS
  "82"= function(imsg, ver) {

    reqId <- imsg$pop()

    sm <- to_matrix(imsg, "SmartComponent")

    validate("smartComponents", reqId=reqId, theMap=sm)
  },

  # NEWS_ARTICLE
  "83"= function(imsg, ver) {

    validate("newsArticle", imsg$pop(3L), no_names=TRUE)
  },

  # TICK_NEWS
  "84"= function(imsg, ver) {

    validate("tickNews", imsg$pop(6L), no_names=TRUE)
  },

  # NEWS_PROVIDERS
  "85"= function(imsg, ver) {

    newsProviders <- to_matrix(imsg, "NewsProvider")

    validate("newsProviders", newsProviders=newsProviders)
  },

  # HISTORICAL_NEWS
  "86"= function(imsg, ver) {

    validate("historicalNews", imsg$pop(5L), no_names=TRUE)
  },

  # HISTORICAL_NEWS_END
  "87"= function(imsg, ver) {

    validate("historicalNewsEnd", imsg$pop(2L), no_names=TRUE)
  },

  # HEAD_TIMESTAMP
  "88"= function(imsg, ver) {

    validate("headTimestamp", imsg$pop(2L), no_names=TRUE)
  },

  # HISTOGRAM_DATA
  "89"= function(imsg, ver) {

    reqId <- imsg$pop()

    d <- to_matrix(imsg, "HistogramData")

    validate("histogramData", reqId=reqId, data=d)
  },

  # HISTORICAL_DATA_UPDATE
  "90"= function(imsg, ver) {

    reqId <- imsg$pop()

    bar <- as.list(structure(imsg$pop(8L),
                             names=c("count", "time", "open", "close", "high", "low", "wap", "volume")))

    validate("historicalDataUpdate", reqId=reqId, bar=bar[c(2L, 3L, 5L, 6L, 4L, 8L, 7L, 1L)])
  },

  # REROUTE_MKT_DATA_REQ
  "91"= function(imsg, ver) {

      validate("rerouteMktDataReq", imsg$pop(3L), no_names=TRUE)
  },

  # REROUTE_MKT_DEPTH_REQ
  "92"= function(imsg, ver) {

    validate("rerouteMktDepthReq", imsg$pop(3L), no_names=TRUE)
  },

  # MARKET_RULE
  "93"= function(imsg, ver) {

    marketRuleId <- imsg$pop()

    p <- to_matrix(imsg, "PriceIncrement")

    validate("marketRule", marketRuleId=marketRuleId, priceIncrements=p)
  },

  # PNL
  "94"= function(imsg, ver) {

    validate("pnl", imsg$pop(4L), no_names=TRUE)
  },

  # PNL_SINGLE
  "95"= function(imsg, ver) {

    validate("pnlSingle", imsg$pop(6L), no_names=TRUE)
  },

  # HISTORICAL_TICKS
  "96"= function(imsg, ver) {

    reqId <- imsg$pop()

    ticks <- to_matrix(imsg, names=c("time", "", "price", "size"))[ , -2L]

    done <- imsg$pop()

    validate("historicalTicks", reqId=reqId, ticks=ticks, done=done)
  },

  # HISTORICAL_TICKS_BID_ASK
  "97"= function(imsg, ver) {

    reqId <- imsg$pop()

    ticks <- to_matrix(imsg, "HistoricalTickBidAsk")

    done <- imsg$pop()

    validate("historicalTicksBidAsk", reqId=reqId, ticks=ticks, done=done)
  },

  # HISTORICAL_TICKS_LAST
  "98"= function(imsg, ver) {

    reqId <- imsg$pop()

    ticks <- to_matrix(imsg, "HistoricalTickLast")

    done <- imsg$pop()

    validate("historicalTicksLast", reqId=reqId, ticks=ticks, done=done)
  },

  # TICK_BY_TICK
  "99"= function(imsg, ver) {

    reqId    <- imsg$pop()
    tickType <- imsg$pop()
    time     <- imsg$pop()

    itype <- Validator$i(tickType)

    if(itype %in% c(1L, 2L)) {

      m <- imsg$pop(5L)

      attrib <- unmask(m[3L], c("pastLimit", "unreported"))

      validate("tickByTickAllLast", reqId=             reqId,
                                    tickType=          tickType,
                                    time=              time,
                                    price=             m[1L],
                                    size=              m[2L],
                                    attribs=           attrib,
                                    exchange=          m[4L],
                                    specialConditions= m[5L])
    }
    else if(itype == 3L) {

      m <- imsg$pop(5L)

      attrib <- unmask(m[5L], c("bidPastLow", "askPastHigh"))

      validate("tickByTickBidAsk", reqId=    reqId,
                                   time=     time,
                                   bidPrice= m[1L],
                                   askPrice= m[2L],
                                   bidSize=  m[3L],
                                   askSize=  m[4L],
                                   attribs=  attrib)
    }
    else if(itype == 4L) {
      validate("tickByTickMidPoint", reqId=    reqId,
                                     time=     time,
                                     midPoint= imsg$pop())
    }
    else {
      warning("TICK_BY_TICK: unknown TickType ", tickType)
      NULL
    }
  },

  # ORDER_BOUND
  "100"= function(imsg, ver) {

    validate("orderBound", imsg$pop(3L), no_names=TRUE)
  },

  # COMPLETED_ORDER
  "101"= function(imsg, ver) {

    contract   <- Contract
    order      <- Order
    orderState <- OrderState

    contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

    order[c(4L:9L)] <- imsg$pop(6L) # "action" -> "tif"

    order[c("ocaGroup",
            "account",
            "openClose")] <- imsg$pop(3L)

    order$origin <- map_int2enum("Origin", imsg$pop())

    order[c("orderRef",
            "permId",
            "outsideRth",
            "hidden",
            "discretionaryAmt",
            "goodAfterTime",
            "faGroup",
            "faMethod",
            "faPercentage",
            "modelCode",
            "goodTillDate",
            "rule80A",
            "percentOffset",
            "settlingFirm",
            "shortSaleSlot",
            "designatedLocation",
            "exemptCode")] <- imsg$pop(17L)

    order[43L:47L] <- imsg$pop(5L) # "startingPrice" -> "stockRangeUpper"

    order[c("displaySize",
            "sweepToFill",
            "allOrNone",
            "minQty",
            "ocaType",
            "triggerMethod")] <- imsg$pop(6L)

    order[50L:53L] <- imsg$pop(4L) # "volatility" -> "deltaNeutralAuxPrice"

    if(nzchar(order$deltaNeutralOrderType))
      order[c(54L, 59L:61L)] <- imsg$pop(4L) # "deltaNeutralConId" -> "deltaNeutralDesignatedLocation"


    order[c("continuousUpdate",
            "referencePriceType",
            "trailStopPrice",
            "trailingPercent")] <- imsg$pop(4L)

    contract$comboLegsDescrip <- imsg$pop()

    # ComboLegs
    contract$comboLegs <- to_list(imsg, "ComboLeg")

    # OrderComboLeg
    orderComboLegsCount <- Validator$i(imsg$pop())

    if(orderComboLegsCount > 0L)
      order$orderComboLegs <- imsg$pop(orderComboLegsCount)

    # SmartComboRouting
    smartComboRoutingParamsCount <- Validator$i(imsg$pop())

    if(smartComboRoutingParamsCount > 0L)
      order$smartComboRoutingParams <- fold_tagvalue(imsg$pop(2L * smartComboRoutingParamsCount))


    order[c("scaleInitLevelSize",
            "scaleSubsLevelSize")] <- imsg$pop(2L)

    order$scalePriceIncrement <- Validator$n(imsg$pop())

    if(!is.na(order$scalePriceIncrement) && order$scalePriceIncrement > 0L)
      order[69L:75L] <- imsg$pop(7L) # "scalePriceAdjustValue" -> "scaleRandomPercent"

    order$hedgeType <- imsg$pop()

    if(nzchar(order$hedgeType))
      order$hedgeParam <- imsg$pop()

    order[c("clearingAccount",
            "clearingIntent",
            "notHeld")] <- imsg$pop(3L)

    # DeltaNeutralContract
    if(Validator$l(imsg$pop())) {

      contract$deltaNeutralContract <- DeltaNeutralContract

      contract$deltaNeutralContract[1L:3L] <- imsg$pop(3L)
    }

    # AlgoStrategy
    order$algoStrategy <- imsg$pop()

    if(nzchar(order$algoStrategy)) {

      algoParamsCount <- Validator$i(imsg$pop())

      if(algoParamsCount > 0L)
        order$algoParams <- fold_tagvalue(imsg$pop(2L * algoParamsCount))
    }

    order$solicited <- imsg$pop()

    orderState$status <- imsg$pop()

    order[c("randomizeSize",
            "randomizePrice")] <- imsg$pop(2L)

    if(order$orderType == "PEG BENCH")
      order[c("referenceContractId",
              "isPeggedChangeAmountDecrease",
              "peggedChangeAmount",
              "referenceChangeAmount",
              "referenceExchangeId")] <- imsg$pop(5L)

    conditionsSize <- Validator$i(imsg$pop())

    if(conditionsSize > 0L) {

      order$conditions <- lapply(seq_len(conditionsSize),
                                  function(i) {

                                    cond <- fCondition(map_int2enum("Condition",
                                                                    Validator$i(imsg$pop())))

                                    cond[-1L] <- imsg$pop(length(cond) - 1L)

                                    cond
                                  })

      order[c("conditionsIgnoreRth",
              "conditionsCancelOrder")] <- imsg$pop(2L)
    }

    order[c("trailStopPrice",
            "lmtPriceOffset",
            "cashQty",
            "dontUseAutoPriceForHedge",
            "isOmsContainer")] <- imsg$pop(5L)

    order[118L:125L] <- imsg$pop(8L) # "autoCancelDate" -> "parentPermId"

    orderState[c("completedTime",
                  "completedStatus")] <- imsg$pop(2L)

    order[c("minTradeQty",
            "minCompeteSize",
            "competeAgainstBestOffset",
            "midOffsetAtWhole",
            "midOffsetAtHalf",
            "customerAccount",
            "professionalCustomer")] <- imsg$pop(7L)

    if(ver >= MIN_SERVER_VER_SUBMITTER)
      order$submitter <- imsg$pop()

    validate("completedOrder", contract=   contract,
                               order=      order,
                               orderState= orderState)
  },

  # COMPLETED_ORDERS_END
  "102"= function(imsg, ver) {

    validate("completedOrdersEnd")
  },

  # REPLACE_FA_END
  "103"= function(imsg, ver) {

    validate("replaceFAEnd", imsg$pop(2L), no_names=TRUE)
  },

  # WSH_META_DATA
  "104"= function(imsg, ver) {

    validate("wshMetaData", imsg$pop(2L), no_names=TRUE)
  },

  # WSH_EVENT_DATA
  "105"= function(imsg, ver) {

    validate("wshEventData", imsg$pop(2L), no_names=TRUE)
  },

  # HISTORICAL_SCHEDULE
  "106"= function(imsg, ver) {

    reqId         <- imsg$pop()
    startDateTime <- imsg$pop()
    endDateTime   <- imsg$pop()
    timeZone      <- imsg$pop()

    sessions <- to_matrix(imsg, "HistoricalSession")

    validate("historicalSchedule", reqId=         reqId,
                                   startDateTime= startDateTime,
                                   endDateTime=   endDateTime,
                                   timeZone=      timeZone,
                                   sessions=      sessions)
  },

  # USER_INFO
  "107"= function(imsg, ver) {

    validate("userInfo", imsg$pop(2L), no_names=TRUE)
  },

  # HISTORICAL_DATA_END
  "108" = function(imsg, ver) {

    validate("historicalDataEnd", imsg$pop(3L), no_names=TRUE)
  },

  # CURRENT_TIME_IN_MILLIS
  "109" = function(imsg, ver) {

    validate("currentTimeInMillis", imsg$pop(), no_names=TRUE)
  },


  #
  # Messages using ProtoBuf have an offset applied to msgid -> msgid + PROTOBUF_MSG_ID = 200
  #

  # EXECUTION_DATA
  "211"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.ExecutionDetails, msg)

    args <- splatpb(pb)

    args$execution$shares <- as.numeric(args$execution$shares)
    args$execution$cumQty <- as.numeric(args$execution$cumQty)
    args$execution$optExerciseOrLapseType <- optexercisetype(args$execution$optExerciseOrLapseType)

    list(fname="execDetails", fargs=args)
  },

  # EXECUTION_DATA_END
  "255"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.ExecutionDetailsEnd, msg)

    list(fname="execDetailsEnd", fargs=splatpb(pb))
  }

))
