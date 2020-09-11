Decoder <- R6::R6Class("Decoder",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  public= list(

    initialize= function(serverVersion) {

      private$serverVersion <- serverVersion
    },

    decode= function(msg) {

      stopifnot(is.character(msg))

      # Make iterator
      imsg <- make_iter(msg)

      # The first field is the message ID
      msgId <- imsg$pop()

      # The second field is unused version, for msgId < 75 and != 3, 5, 11, 17, 21
      imsgId <- Validator$i(msgId)
      if(imsgId  < 75L && ! imsgId %in% c(3L, 5L, 11L, 17L, 21L) ||
         imsgId ==  5L && private$serverVersion < MIN_SERVER_VER_ORDER_CONTAINER ||
         imsgId == 21L && private$serverVersion < MIN_SERVER_VER_PRICE_BASED_VOLATILITY)
        imsg$pop()

      # Convert ID -> Name
      msgName <- map_inbound[msgId]

      # Unknown msgId
      res <- if(is.na(msgName)) {
               warning("Unknown message id: ", msgId)
               NULL
             }
             else
               # Call the appropriate handler
               private[[msgName]](imsg)

      # Check that all the message has been processed
      if(imsg$left() > 0L)
        warning("Message: ", msgName, " not completely processed")

      res
    }

  ),

  private= list(

    serverVersion= NULL,  # Server Version

    #
    # Validate/convert args
    #
    validate= function(fname, ..., no_names=FALSE) {

      args <- list(...)

      # Handle parameters passed in a single character vector
      if(length(args) == 1L && is.character(args[[1L]]) && length(args[[1L]]) > 1L && !is.matrix(args[[1L]]))
        args <- as.list(args[[1L]])

      if(length(args) > 0L)
        args <- Validator[[fname]](args, no_names=no_names)

      # Return callback name and argument list
      list(fname=fname, fargs=args)
    },

    #
    # Make a matrix by row
    #
    to_matrix= function(imsg, n, struct) {

      names <- get("names", environment(Validator[[struct]]))

      matrix(imsg$pop(n * length(names)),
             ncol=     length(names),
             byrow=    TRUE,
             dimnames= list(character(), names))
    },

    #
    # Unmask "mask" into a list of logicals
    #
    unmask= function(mask, names) {

      as.list(structure(intToBits(mask)[seq_along(names)] != 0L,
                        names=names))
    },

    # ##############################################################
    #
    # Message handlers
    #
    # ##############################################################

    TICK_PRICE= function(imsg) {

      m <- imsg$pop(5L)

      # Convert tickType to string
      m[2L] <- map_ticktype[m[2L]]

      attrib <- private$unmask(m[5L], c("canAutoExecute", "pastLimit", "preOpen"))

      private$validate("tickPrice", tickerId= m[1L],
                                    field=    m[2L],
                                    price=    m[3L],
                                    size=     m[4L],
                                    attrib=   attrib)
    },

    TICK_SIZE= function(imsg) {

      m <- imsg$pop(3L)

      # Convert tickType to string
      m[2L] <- map_ticktype[m[2L]]

      private$validate("tickSize",  m, no_names=TRUE)
    },

    TICK_OPTION_COMPUTATION= function(imsg) {

      m <- if(private$serverVersion >= MIN_SERVER_VER_PRICE_BASED_VOLATILITY)
             imsg$pop(11L)
           else
             c(imsg$pop(2L), NA_character_, imsg$pop(8L))

      # Convert tickType to string
      m[2L] <- map_ticktype[m[2L]]

      # (impliedVol, optPrice, pvDividend, undPrice) == -1 means NA
      idx <- c(4L, 6L, 7L, 11L)
      m[idx][m[idx] == "-1"] <- NA_character_

      # (delta, gamma, vega, theta) == -2 means NA
      idx <- c(5L, 8L, 9L, 10L)
      m[idx][m[idx] == "-2"] <- NA_character_

      private$validate("tickOptionComputation", m, no_names=TRUE)
    },

    TICK_GENERIC= function(imsg) {

      m <- imsg$pop(3L)

      # Convert tickType to string
      m[2L] <- map_ticktype[m[2L]]

      private$validate("tickGeneric", m, no_names=TRUE)
    },

    TICK_STRING= function(imsg) {

      m <- imsg$pop(3L)

      # Convert tickType to string
      m[2L] <- map_ticktype[m[2L]]

      private$validate("tickString", m, no_names=TRUE)
    },

    TICK_EFP= function(imsg) {

      m <- imsg$pop(9L)

      # Convert tickType to string
      m[2L] <- map_ticktype[m[2L]]

      private$validate("tickEFP", m, no_names=TRUE)
    },

    ORDER_STATUS= function(imsg) {

      private$validate("orderStatus", imsg$pop(11L), no_names=TRUE)
    },

    ERR_MSG= function(imsg) {

      private$validate("error", imsg$pop(3L), no_names=TRUE)
    },

    OPEN_ORDER= function(imsg) {

      order    <- Order
      contract <- Contract

      order$orderId <- imsg$pop()

      contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

      order[c(4L:9L)] <- imsg$pop(6L)  # "action" through "tif"

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

      # Skip msg[32] "sharesAllocation"
      imsg$pop()

      order[c("faGroup",
              "faMethod",
              "faPercentage",
              "faProfile",
              "modelCode",
              "goodTillDate",
              "rule80A",
              "percentOffset",
              "settlingFirm",
              "shortSaleSlot",
              "designatedLocation",
              "exemptCode")] <- imsg$pop(12L)

      order$auctionStrategy <- map_int2enum("AuctionStrategy", imsg$pop())

      order[47L:51L] <- imsg$pop(5L)     # "startingPrice" through "stockRangeUpper"

      order[c("displaySize",
              "blockOrder",
              "sweepToFill",
              "allOrNone",
              "minQty",
              "ocaType",
              "eTradeOnly",
              "firmQuoteOnly",
              "nbboPriceCap",
              "parentId",
              "triggerMethod")] <- imsg$pop(11L)

      order[54L:57L] <- imsg$pop(4L)   # "volatility" through "deltaNeutralAuxPrice"

      if(nzchar(order$deltaNeutralOrderType))
        order[58L:65L] <- imsg$pop(8L)  # "deltaNeutralConId" through "deltaNeutralDesignatedLocation"


      order[c("continuousUpdate",
              "referencePriceType",
              "trailStopPrice",
              "trailingPercent",
              "basisPoints",
              "basisPointsType")] <- imsg$pop(6L)

      contract$comboLegsDescrip <- imsg$pop()

      # ComboLegs
      comboLegsCount <- Validator$i(imsg$pop())

      if(comboLegsCount > 0L) {

        contract$comboLegs <- lapply(seq_len(comboLegsCount),
                                     function(i) {

                                        combo <- ComboLeg

                                        combo[1L:8L] <- imsg$pop(8L)

                                        combo
                                      })
      }

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
        order[73L:79L] <- imsg$pop(7L)


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

      orderState <- OrderState

      orderState[1L:15L] <- imsg$pop(15L)

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

        for(i in seq_len(conditionsSize)) {

          condition <- fCondition(map_int2enum("Condition",
                                                Validator$i(imsg$pop())))

          condition[-1L] <- imsg$pop(length(condition) - 1L)

          order$conditions[[i]] <- condition
        }

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
              "dontUseAutoPriceForHedge")] <- imsg$pop(2L)

      if(private$serverVersion >= MIN_SERVER_VER_ORDER_CONTAINER)
        order$isOmsContainer <- imsg$pop()

      if(private$serverVersion >= MIN_SERVER_VER_D_PEG_ORDERS)
        order$discretionaryUpToLimitPrice <- imsg$pop()

      if(private$serverVersion >= MIN_SERVER_VER_PRICE_MGMT_ALGO)
        order$usePriceMgmtAlgo <- imsg$pop()

      private$validate("openOrder", orderId=    order$orderId,
                                    contract=   contract,
                                    order=      order,
                                    orderstate= orderState)
    },

    ACCT_VALUE= function(imsg) {

      private$validate("updateAccountValue", imsg$pop(4L), no_names=TRUE)
    },

    PORTFOLIO_VALUE= function(imsg) {

      contract <- Contract

      contract[c(1L:7L, 9L:12L)] <- imsg$pop(11L)
      position       <- imsg$pop()
      marketPrice    <- imsg$pop()
      marketValue    <- imsg$pop()
      averageCost    <- imsg$pop()
      unrealizedPNL  <- imsg$pop()
      realizedPNL    <- imsg$pop()
      accountName    <- imsg$pop()

      private$validate("updatePortfolio", contract=      contract,
                                          position=      position,
                                          marketPrice=   marketPrice,
                                          marketValue=   marketValue,
                                          averageCost=   averageCost,
                                          unrealizedPNL= unrealizedPNL,
                                          realizedPNL=   realizedPNL,
                                          accountName=   accountName)
    },

    ACCT_UPDATE_TIME= function(imsg) {

      private$validate("updateAccountTime", timeStamp=imsg$pop())
    },

    NEXT_VALID_ID= function(imsg) {

      private$validate("nextValidId", orderId=imsg$pop())
    },

    CONTRACT_DATA= function(imsg) {

      reqId <- imsg$pop()

      cd <- ContractDetails

      cd$contract[c(2L:6L, 8L, 10L, 11L)] <- imsg$pop(8L)
      cd$marketName                       <- imsg$pop()
      cd$contract$tradingClass            <- imsg$pop()
      cd$contract$conId                   <- imsg$pop()
      cd$minTick                          <- imsg$pop()
      cd$mdSizeMultiplier                 <- imsg$pop()
      cd$contract$multiplier              <- imsg$pop()
      cd[4L:8L]                           <- imsg$pop(5L)
      cd$contract$primaryExchange         <- imsg$pop()
      cd[9L:17L]                          <- imsg$pop(9L)

      secIdListcount <- Validator$i(imsg$pop())

      if(secIdListcount > 0L)
        cd$secIdList <- fold_tagvalue(imsg$pop(2L * secIdListcount))

      cd[c("aggGroup",
           "underSymbol",
           "underSecType",
           "marketRuleIds",
           "realExpirationDate")] <- imsg$pop(5L)

      if(private$serverVersion >= MIN_SERVER_VER_STOCK_TYPE)
        cd$stockType <- imsg$pop()

      private$validate("contractDetails", reqId=reqId, contractDetails=cd)
    },

    BOND_CONTRACT_DATA= function(imsg) {

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
           "mdSizeMultiplier",
           "orderTypes",
           "validExchanges",
           "nextOptionDate",
           "nextOptionType",
           "nextOptionPartial",
           "notes",
           "longName",
           "evRule",
           "evMultiplier")] <- imsg$pop(11L)

      secIdListCount <- Validator$i(imsg$pop())

      if(secIdListcount > 0L)
        cd$secIdList <- fold_tagvalue(imsg$pop(2L * secIdListcount))

      cd[c("aggGroup",
           "marketRuleIds")] <- imsg$pop(2L)

      private$validate("bondContractDetails", reqId=reqId, contractDetails=cd)
    },

    EXECUTION_DATA= function(imsg) {

      contract  <- Contract
      execution <- Execution

      reqId <- imsg$pop()

      execution$orderId <- imsg$pop()

      contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

      execution[c(1L:9L, 11L:18L)] <- imsg$pop(17L)

      private$validate("execDetails", reqId=reqId, contract=contract, execution=execution)
    },

    MARKET_DEPTH= function(imsg) {

      private$validate("updateMktDepth", imsg$pop(6L), no_names=TRUE)
    },

    MARKET_DEPTH_L2= function(imsg) {

      m <- if(private$serverVersion >= MIN_SERVER_VER_SMART_DEPTH)
             imsg$pop(8L)
           else
             c(imsg$pop(7L), "0")

      private$validate("updateMktDepthL2", m, no_names=TRUE)
    },

    NEWS_BULLETINS= function(imsg) {

      private$validate("updateNewsBulletin", imsg$pop(4L), no_names=TRUE)
    },

    MANAGED_ACCTS= function(imsg) {

      private$validate("managedAccounts", accountsList=imsg$pop())
    },

    RECEIVE_FA= function(imsg) {

      faDataType <- map_int2enum("FaDataType", imsg$pop())

      private$validate("receiveFA", faDataType=faDataType, xml=imsg$pop())
    },

    HISTORICAL_DATA= function(imsg) {

      reqId <- imsg$pop()

      # Ignore startDate, endDate
      imsg$pop(2L)

      # Number of rows
      n <- Validator$i(imsg$pop())

      bar <- matrix(imsg$pop(n * 8L), ncol=8L, byrow=TRUE)

      dimnames(bar) <- list(character(), c("time", "open", "high", "low", "close", "volume", "wap", "count"))

      private$validate("historicalData",  reqId=reqId, bar=bar)
    },

    SCANNER_DATA= function(imsg) {

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

      private$validate("scannerData", reqId=           reqId,
                                      rank=            rank,
                                      contractDetails= cd,
                                      distance=        distance,
                                      benchmark=       benchmark,
                                      projection=      projection,
                                      legsStr=         legsStr)
    },

    SCANNER_PARAMETERS= function(imsg) {

      private$validate("scannerParameters", xml=imsg$pop())
    },

    CURRENT_TIME= function(imsg) {

      private$validate("currentTime", time=imsg$pop())
    },

    REAL_TIME_BARS= function(imsg) {

      private$validate("realtimeBar", imsg$pop(9L), no_names=TRUE)
    },

    FUNDAMENTAL_DATA= function(imsg) {

      reqId <- imsg$pop()

      private$validate("fundamentalData", reqId=reqId, data=imsg$pop())
    },

    CONTRACT_DATA_END= function(imsg) {

      private$validate("contractDetailsEnd", reqId=imsg$pop())
    },

    OPEN_ORDER_END= function(imsg) {

      private$validate("openOrderEnd")
    },

    ACCT_DOWNLOAD_END= function(imsg) {

      private$validate("accountDownloadEnd", accountName=imsg$pop())
    },

    EXECUTION_DATA_END= function(imsg) {

      private$validate("execDetailsEnd", reqId=imsg$pop())
    },

    DELTA_NEUTRAL_VALIDATION= function(imsg) {

      reqId <- imsg$pop()

      deltaNeutralContract <- DeltaNeutralContract

      deltaNeutralContract[1L:3L] <- imsg$pop(3L)

      private$validate("deltaNeutralValidation", reqId=reqId, deltaNeutralContract=deltaNeutralContract)
    },

    TICK_SNAPSHOT_END= function(imsg) {

      private$validate("tickSnapshotEnd", reqId=imsg$pop())
    },

    MARKET_DATA_TYPE= function(imsg) {

      reqId <- imsg$pop()

      private$validate("marketDataType", reqId=reqId, marketDataType=imsg$pop())
    },

    COMMISSION_REPORT= function(imsg) {

      commission <- CommissionReport

      commission[1L:6L] <- imsg$pop(6L)

      private$validate("commissionReport", commissionReport=commission)
    },

    POSITION_DATA= function(imsg) {

      account <- imsg$pop()

      contract <- Contract
      contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

      position <- imsg$pop()
      avgCost  <- imsg$pop()

      private$validate("position",  account=  account,
                                    contract= contract,
                                    position= position,
                                    avgCost=  avgCost)
    },

    POSITION_END= function(imsg) {

      private$validate("positionEnd")
    },

    ACCOUNT_SUMMARY= function(imsg) {

      private$validate("accountSummary", imsg$pop(5L), no_names=TRUE)
    },

    ACCOUNT_SUMMARY_END= function(imsg) {

      private$validate("accountSummaryEnd", reqId=imsg$pop())
    },

    VERIFY_MESSAGE_API= function(imsg) {

      private$validate("verifyMessageAPI", apiData=imsg$pop())
    },

    VERIFY_COMPLETED= function(imsg) {

      isSuccessful <- imsg$pop()
      private$validate("verifyCompleted", isSuccessful=isSuccessful, errorText=imsg$pop())
    },

    DISPLAY_GROUP_LIST= function(imsg) {

      reqId <- imsg$pop()

      private$validate("displayGroupList", reqId=reqId, groups=imsg$pop())
    },

    DISPLAY_GROUP_UPDATED= function(imsg) {

      reqId <- imsg$pop()

      private$validate("displayGroupUpdated", reqId=reqId, contractInfo=imsg$pop())
    },

    VERIFY_AND_AUTH_MESSAGE_API= function(imsg) {

      apiData <- imsg$pop()

      private$validate("verifyAndAuthMessageAPI", apiData=apiData, xyzChallenge=imsg$pop())
    },

    VERIFY_AND_AUTH_COMPLETED= function(imsg) {

      isSuccessful <- imsg$pop()

      private$validate("verifyAndAuthCompleted", isSuccessful=isSuccessful, errorText=imsg$pop())
    },

    POSITION_MULTI= function(imsg) {

      reqId   <- imsg$pop()
      account <- imsg$pop()

      contract <- Contract
      contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

      position  <- imsg$pop()
      avgCost   <- imsg$pop()
      modelCode <- imsg$pop()

      private$validate("positionMulti", reqId=     reqId,
                                        account=   account,
                                        modelCode= modelCode,
                                        contract=  contract,
                                        position=  position,
                                        avgCost=   avgCost)
    },

    POSITION_MULTI_END= function(imsg) {

      private$validate("positionMultiEnd", reqId=imsg$pop())
    },

    ACCOUNT_UPDATE_MULTI= function(imsg) {

      private$validate("accountUpdateMulti", imsg$pop(6L), no_names=TRUE)
    },

    ACCOUNT_UPDATE_MULTI_END= function(imsg) {

      private$validate("accountUpdateMultiEnd", reqId=imsg$pop())
    },

    SECURITY_DEFINITION_OPTION_PARAMETER= function(imsg) {

      reqId           <- imsg$pop()
      exchange        <- imsg$pop()
      underlyingConId <- imsg$pop()
      tradingClass    <- imsg$pop()
      multiplier      <- imsg$pop()

      nExp  <- Validator$i(imsg$pop())
      expirations <- imsg$pop(nExp)

      nStrk <- Validator$i(imsg$pop())
      strikes <- imsg$pop(nStrk)

      private$validate("securityDefinitionOptionalParameter", reqId=           reqId,
                                                              exchange=        exchange,
                                                              underlyingConId= underlyingConId,
                                                              tradingClass=    tradingClass,
                                                              multiplier=      multiplier,
                                                              expirations=     expirations,
                                                              strikes=         strikes)
    },

    SECURITY_DEFINITION_OPTION_PARAMETER_END= function(imsg) {

      private$validate("securityDefinitionOptionalParameterEnd", reqId=imsg$pop())
    },

    SOFT_DOLLAR_TIERS= function(imsg) {

      reqId <- imsg$pop()

      n <- Validator$i(imsg$pop())

      tiers <- private$to_matrix(imsg, n, "SoftDollarTier")

      private$validate("softDollarTiers", reqId=reqId, tiers=tiers)
    },

    FAMILY_CODES= function(imsg) {

      n <- Validator$i(imsg$pop())

      familyCodes <- private$to_matrix(imsg, n, "FamilyCode")

      private$validate("familyCodes", familyCodes=familyCodes)
    },

    SYMBOL_SAMPLES= function(imsg) {

      reqId <- imsg$pop()

      cds <- lapply(seq_len(Validator$i(imsg$pop())), function(i) {

                              cd <- ContractDescription

                              cd$contract[c("conId",
                                            "symbol",
                                            "secType",
                                            "primaryExchange",
                                            "currency")] <- imsg$pop(5L)

                              nsec <- Validator$i(imsg$pop())

                              if(nsec>0L)
                                cd$derivativeSecTypes <- imsg$pop(nsec)

                              cd
                            })

      private$validate("symbolSamples", reqId=reqId, contractDescriptions=cds)
    },

    MKT_DEPTH_EXCHANGES= function(imsg) {

      n <- Validator$i(imsg$pop())

      dms <- private$to_matrix(imsg, n, "DepthMktDataDescription")

      private$validate("mktDepthExchanges", depthMktDataDescriptions=dms)
    },

    TICK_REQ_PARAMS= function(imsg) {

      private$validate("tickReqParams", imsg$pop(4L), no_names=TRUE)
    },

    SMART_COMPONENTS= function(imsg) {

      reqId <- imsg$pop()

      n <- Validator$i(imsg$pop())

      sm <- private$to_matrix(imsg, n, "SmartComponent")

      private$validate("smartComponents", reqId=reqId, theMap=sm)
    },

    NEWS_ARTICLE= function(imsg) {

      private$validate("newsArticle", imsg$pop(3L), no_names=TRUE)
    },

    TICK_NEWS= function(imsg) {

      private$validate("tickNews", imsg$pop(6L), no_names=TRUE)
    },

    NEWS_PROVIDERS= function(imsg) {

      n <- Validator$i(imsg$pop())

      newsProviders <- private$to_matrix(imsg, n, "NewsProvider")

      private$validate("newsProviders", newsProviders=newsProviders)
    },

    HISTORICAL_NEWS= function(imsg) {

      private$validate("historicalNews", imsg$pop(5L), no_names=TRUE)
    },

    HISTORICAL_NEWS_END= function(imsg) {

      private$validate("historicalNewsEnd", imsg$pop(2L), no_names=TRUE)
    },

    HEAD_TIMESTAMP= function(imsg) {

      private$validate("headTimestamp", imsg$pop(2L), no_names=TRUE)
    },

    HISTOGRAM_DATA= function(imsg) {

      reqId <- imsg$pop()

      n <- Validator$i(imsg$pop())

      d <- private$to_matrix(imsg, n, "HistogramData")

      private$validate("histogramData", reqId=reqId, data=d)
    },

    HISTORICAL_DATA_UPDATE= function(imsg) {

      reqId <- imsg$pop()

      bar <- as.list(structure(imsg$pop(8L),
                               names=c("count", "time", "open", "close", "high", "low", "wap", "volume")))

      private$validate("historicalDataUpdate", reqId=reqId, bar=bar[c(2L, 3L, 5L, 6L, 4L, 8L, 7L, 1L)])
    },

    REROUTE_MKT_DATA_REQ= function(imsg) {

       private$validate("rerouteMktDataReq", imsg$pop(3L), no_names=TRUE)
    },

    REROUTE_MKT_DEPTH_REQ= function(imsg) {

      private$validate("rerouteMktDepthReq", imsg$pop(3L), no_names=TRUE)
    },

    MARKET_RULE= function(imsg) {

      marketRuleId <- imsg$pop()

      n <- Validator$i(imsg$pop())

      p <- private$to_matrix(imsg, n, "PriceIncrement")

      private$validate("marketRule", marketRuleId=marketRuleId, priceIncrements=p)
    },

    PNL= function(imsg) {

      private$validate("pnl", imsg$pop(4L), no_names=TRUE)
    },

    PNL_SINGLE= function(imsg) {

      private$validate("pnlSingle", imsg$pop(6L), no_names=TRUE)
    },

    HISTORICAL_TICKS= function(imsg) {

      reqId <- imsg$pop()

      n <- Validator$i(imsg$pop())

      ticks <- matrix(imsg$pop(n * 4L),
                      ncol=     4L,
                      byrow=    TRUE,
                      dimnames= list(character(), c("time", "", "price", "size")))[ , -2L]

      done <- imsg$pop()

      private$validate("historicalTicks", reqId=reqId, ticks=ticks, done=done)
    },

    HISTORICAL_TICKS_BID_ASK= function(imsg) {

      reqId <- imsg$pop()

      n <- Validator$i(imsg$pop())

      ticks <- private$to_matrix(imsg, n, "HistoricalTickBidAsk")

      done <- imsg$pop()

      private$validate("historicalTicksBidAsk", reqId=reqId, ticks=ticks, done=done)
    },

    HISTORICAL_TICKS_LAST= function(imsg) {

      reqId <- imsg$pop()

      n <- Validator$i(imsg$pop())

      ticks <- private$to_matrix(imsg, n, "HistoricalTickLast")

      done <- imsg$pop()

      private$validate("historicalTicksLast", reqId=reqId, ticks=ticks, done=done)
    },

    TICK_BY_TICK= function(imsg) {

      reqId    <- imsg$pop()
      tickType <- imsg$pop()
      time     <- imsg$pop()

      itype <- Validator$i(tickType)

      if(itype %in% c(1L, 2L)) {

        m <- imsg$pop(5L)

        attrib <- private$unmask(m[3L], c("pastLimit", "unreported"))

        private$validate("tickByTickAllLast", reqId=             reqId,
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

        attrib <- private$unmask(m[5L], c("bidPastLow", "askPastHigh"))

        private$validate("tickByTickBidAsk", reqId=    reqId,
                                             time=     time,
                                             bidPrice= m[1L],
                                             askPrice= m[2L],
                                             bidSize=  m[3L],
                                             askSize=  m[4L],
                                             attribs=  attrib)
      }
      else if(itype == 4L) {
        private$validate("tickByTickMidPoint", reqId=    reqId,
                                               time=     time,
                                               midPoint= imsg$pop())
      }
      else {
        warning("TICK_BY_TICK: Unknown TickType ", tickType)
        NULL
      }
    },

    ORDER_BOUND= function(imsg) {

      private$validate("orderBound", imsg$pop(3L), no_names=TRUE)
    },

    COMPLETED_ORDER= function(imsg) {

      contract   <- Contract
      order      <- Order
      orderState <- OrderState

      contract[c(1L:8L, 10L:12L)] <- imsg$pop(11L)

      order[c(4L:9L)] <- imsg$pop(6L)  # "action" through "tif"

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
              "faProfile",
              "modelCode",
              "goodTillDate",
              "rule80A",
              "percentOffset",
              "settlingFirm",
              "shortSaleSlot",
              "designatedLocation",
              "exemptCode")] <- imsg$pop(18L)

      order[47L:51L] <- imsg$pop(5L)     # "startingPrice" through "stockRangeUpper"

      order[c("displaySize",
              "sweepToFill",
              "allOrNone",
              "minQty",
              "ocaType",
              "triggerMethod")] <- imsg$pop(6L)

      order[54L:57L] <- imsg$pop(4L)   # "volatility" through "deltaNeutralAuxPrice"

      if(nzchar(order$deltaNeutralOrderType))
        order[c(58L, 63L:65L)] <- imsg$pop(4L)  # "deltaNeutralConId" through "deltaNeutralDesignatedLocation"


      order[c("continuousUpdate",
              "referencePriceType",
              "trailStopPrice",
              "trailingPercent")] <- imsg$pop(4L)

      contract$comboLegsDescrip <- imsg$pop()

      # ComboLegs
      comboLegsCount <- Validator$i(imsg$pop())

      if(comboLegsCount > 0L) {

        contract$comboLegs <- lapply(seq_len(comboLegsCount),
                                     function(i) {

                                        combo <- ComboLeg

                                        combo[1L:8L] <- imsg$pop(8L)

                                        combo
                                      })
      }

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
        order[73L:79L] <- imsg$pop(7L)


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

        for(i in seq_len(conditionsSize)) {

          condition <- fCondition(map_int2enum("Condition",
                                                Validator$i(imsg$pop())))

          condition[-1L] <- imsg$pop(length(condition) - 1L)

          order$conditions[[i]] <- condition
        }

        order[c("conditionsIgnoreRth",
                "conditionsCancelOrder")] <- imsg$pop(2L)
      }

      order[c("trailStopPrice",
              "lmtPriceOffset",
              "cashQty",
              "dontUseAutoPriceForHedge")] <- imsg$pop(4L)

      if(private$serverVersion >= MIN_SERVER_VER_ORDER_CONTAINER)
        order$isOmsContainer <- imsg$pop()

      order[122L:129L] <- imsg$pop(8L)     # "autoCancelDate" through "parentPermId"

      orderState[c("completedTime",
                   "completedStatus")] <- imsg$pop(2L)

      private$validate("completedOrder", contract=   contract,
                                         order=      order,
                                         orderState= orderState)
    },

    COMPLETED_ORDERS_END= function(imsg) {

      private$validate("completedOrdersEnd")
    },

    REPLACE_FA_END= function(imsg) {

      private$validate("replaceFAEnd", imsg$pop(2L), no_names=TRUE)
    }
  )
)
