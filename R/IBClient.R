IBClient <- R6Class("IBClient",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  private= list(

    socket=          NULL,  # Socket connection

    serverVersion=   NULL,  # Returned on connection
    serverTimestamp= NULL,  # Returned on connection

    id=              NULL,  # Client ID

    decoder=         NULL,  # Decoder

    #
    # Finalizer
    #
    finalize= function() {

      self$disconnect()
    },

    #
    # Encode and send a message
    #
    # msg: character vector
    #
    encodeMsg= function(msg, api_sign=FALSE) {

      stopifnot(is.character(msg),
                !is.na(msg))

      raw_msg <- writeBin(msg, raw())

      if(api_sign) {
        # Chop last '\0'
        raw_msg <- head(raw_msg, -1L)
      }

      len <- length(raw_msg)

      stopifnot(raw_msg < as.raw(0x80L),   # Only ASCII chars are allowed
                len <= MAX_MSG_LEN)

      header <- writeBin(len, raw(), size=HEADER_LEN, endian="big")

      res <- if(api_sign) c(API_SIGN, header, raw_msg)
             else         c(header, raw_msg)

      # Write to socket
      writeBin(res, private$socket)
    },

    #
    # Read one message. BLOCKING
    #
    # Return the fields in a character vector
    #
    readOneMsg= function() {

      # Read header and decode message length
      len <- readBin(private$socket, integer(), size=HEADER_LEN, endian="big")

      # Invalid socket
      if(length(len) == 0L)
        stop("lost connection")

      # Header consistency check
      stopifnot(len >  0L,
                len <= MAX_MSG_LEN)

      raw_msg <- readBin(private$socket, raw(), n=len)

      # Count the fields
      n <- sum(raw_msg == as.raw(0L))

      # Consistency checks
      stopifnot(length(raw_msg) == len,      # Entire message is read
                raw_msg[len] == as.raw(0L),  # Last byte is 0x00
                n > 0L)                      # At least 1 field

      readBin(raw_msg, character(), n=n)
    },

    # Return character vector
    sanitize= function(v) {

      stopifnot(is.list(v))

      # Convert NA -> ""
      v[is.na(v)] <- ""

      # Convert bool -> integer (0,1)
      if(length(idx <- which(vapply(v, is.logical, NA))) > 0L)
        v[idx] <- as.integer(v[idx])

      # Convert Inf -> "Infinity"
      if(length(idx <- which(vapply(v, function(x) x == Inf, NA))) > 0L)
        v[idx] <- "Infinity"

      as.character(v)
    },

    # Convenience wrapper for simple payload requests
    req_simple= function(msgid, ...)
                  private$encodeMsg(c(msgid, ...))
  ),

  active= list(

    serVersion=   function() private$serverVersion,

    serTimestamp= function() private$serverTimestamp,

    clientId=     function() private$id
  ),

  public= list(

    connect= function(host="localhost", port=4002L, clientId=1L, connectOptions="", optionalCapabilities="") {

      stopifnot(is.null(private$socket))

      # Open connection to server
      private$socket <- socketConnection(host=host, port=port, open="r+b", blocking=TRUE)

      # Prefix " " to connectOptions, if not empty
      if(nzchar(connectOptions, keepNA=TRUE))
        connectOptions <- paste0(" ", connectOptions)

      # Start handshake
      private$encodeMsg(paste0("v", MIN_CLIENT_VER, "..", MAX_CLIENT_VER, connectOptions), api_sign=TRUE)

      # Server response
      res <- private$readOneMsg()

      stopifnot(length(res) == 2L)

      message("server version: ", res[1L], " timestamp: ", res[2L])
      private$serverVersion   <- as.integer(res[1L])
      private$serverTimestamp <- res[2L]

      # Check server version
      if(private$serverVersion < MIN_CLIENT_VER ||
         private$serverVersion > MAX_CLIENT_VER) {

        self$disconnect()
        stop("unsupported version")
      }

      # startAPI
      self$startApi(clientId, optionalCapabilities)

      private$id <- clientId

      # TODO
      # Verify that connection was successful

      # Instantiate Decoder
      private$decoder <- Decoder$new(private$serverVersion)
    },

    disconnect= function() {

      if(!is.null(private$socket)) {

        close(private$socket)

        private$socket          <-
        private$serverVersion   <-
        private$serverTimestamp <-
        private$id              <-
        private$decoder         <- NULL
      }
    },

    #
    # Process server responses
    #
    # Block up to timeout
    # If wrap is missing, messages are discarded
    # otherwise callbacks are dispatched
    #
    checkMsg= function(wrap, timeout=0.2) {

      count <- 0L

      while(socketSelect(list(private$socket), write=FALSE, timeout=timeout)) {

        count <- count + 1L

        msg <- private$readOneMsg()

        if(!missing(wrap)) {

          # Decode message
          res <- private$decoder$decode(msg)

          # Dispatch callback
          if(!is.null(res))
            do.call(wrap[[res$fname]], res$fargs)
        }
      }

      count
    },

    # ########################################################################
    #
    # Send requests to the server
    #
    # ########################################################################

    reqMktData= function(tickerId, contract, genericTicks, snapshot, regulatorySnaphsot=FALSE, mktDataOptions=character()) {

      msg <- c("1", "11") ### REQ_MKT_DATA

      payload <- c(contract[1L:12L],

                   if(contract$secType == "BAG")
                     c(length(contract$comboLegs),
                       do.call(c, lapply(contract$comboLegs, function(l) l[1L:4L]))),

                   if(is.list(contract$deltaNeutralContract))
                     c(TRUE, contract$deltaNeutralContract)
                   else
                     FALSE,

                   genericTicks,
                   snapshot,
                   regulatorySnaphsot,
                   pack_tagvalue(mktDataOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    cancelMktData= function(tickerId) private$req_simple("2", "2", tickerId), ### CANCEL_MKT_DATA

    placeOrder= function(id, contract, order) {

      msg <- "3" ### PLACE_ORDER

      payload <- c(contract[c(1L:12L, 14L, 15L)],

                   order[4L:9L], # "action" -> "tif"

                   order[c("ocaGroup",
                           "account",
                           "openClose")],

                   map_enum2int("Origin", order$origin),

                   order[14L:22L], # "orderRef" -> "hidden"

                   if(contract$secType == "BAG")
                     c(length(contract$comboLegs),
                       do.call(c, contract$comboLegs),

                       length(order$orderComboLegs),
                       order$orderComboLegs,

                       length(order$smartComboRoutingParams),
                       pack_tagvalue(order$smartComboRoutingParams, mode="unfold")),

                   "", # Deprecated sharesAllocation

                   order[c("discretionaryAmt",
                           "goodAfterTime",
                           "goodTillDate",
                           "faGroup",
                           "faMethod",
                           "faPercentage")],

                   if(self$serVersion < MIN_SERVER_VER_FA_PROFILE_DESUPPORT)
                     "", # Deprecated faProfile

                   order[c("modelCode",
                           "shortSaleSlot",
                           "designatedLocation",
                           "exemptCode",
                           "ocaType",
                           "rule80A",
                           "settlingFirm",
                           "allOrNone",
                           "minQty",
                           "percentOffset")],

                   FALSE, # Deprecated eTradeOnly
                   FALSE, # Deprecated firmQuoteOnly
                   "",    # Deprecated nbboPriceCap

                   map_enum2int("AuctionStrategy", order$auctionStrategy),

                   order[c("startingPrice",
                           "stockRefPrice",
                           "delta",
                           "stockRangeLower",
                           "stockRangeUpper",
                           "overridePercentageConstraints",
                           "volatility",
                           "volatilityType",
                           "deltaNeutralOrderType",
                           "deltaNeutralAuxPrice")],

                   if(nzchar(order$deltaNeutralOrderType, keepNA=TRUE))
                     order[54L:61L], # "deltaNeutralConId" -> "deltaNeutralDesignatedLocation"

                   order[c("continuousUpdate",
                           "referencePriceType",
                           "trailStopPrice",
                           "trailingPercent",
                           "scaleInitLevelSize",
                           "scaleSubsLevelSize",
                           "scalePriceIncrement")],

                   if(!is.na(order$scalePriceIncrement) &&
                      order$scalePriceIncrement > 0)
                     order[69L:75L], # "scalePriceAdjustValue" -> "scaleRandomPercent"

                   order[c("scaleTable",
                           "activeStartTime",
                           "activeStopTime",
                           "hedgeType")],

                   if(nzchar(order$hedgeType, keepNA=TRUE))
                     order$hedgeParam,

                   order[c("optOutSmartRouting",
                           "clearingAccount",
                           "clearingIntent",
                           "notHeld")],

                   if(is.list(contract$deltaNeutralContract))
                     c(TRUE, contract$deltaNeutralContract)
                   else
                     FALSE,

                   order$algoStrategy,

                   if(nzchar(order$algoStrategy, keepNA=TRUE))
                     c(length(order$algoParams),
                       pack_tagvalue(order$algoParams, mode="unfold")),

                   order[c("algoId",
                           "whatIf")],

                  pack_tagvalue(order$orderMiscOptions, mode="string"),

                   order[c("solicited",
                           "randomizeSize",
                           "randomizePrice")],

                   if(order$orderType == "PEG BENCH")
                     order[c("referenceContractId",
                             "isPeggedChangeAmountDecrease",
                             "peggedChangeAmount",
                             "referenceChangeAmount",
                             "referenceExchangeId")],

                   length(order$conditions),

                   if(length(order$conditions) > 0L)
                     c(do.call(c, lapply(order$conditions,
                                         function(cond)
                                           c(map_enum2int("Condition", cond$type), cond[-1L]))),
                       order[c("conditionsIgnoreRth",
                               "conditionsCancelOrder")]),

                   order[c("adjustedOrderType",
                           "triggerPrice",
                           "lmtPriceOffset",
                           "adjustedStopPrice",
                           "adjustedStopLimitPrice",
                           "adjustedTrailingAmount",
                           "adjustableTrailingUnit",
                           "extOperator")],

                   order$softDollarTier[c("name", "val")],

                   order[c("cashQty",
                           "mifid2DecisionMaker",
                           "mifid2DecisionAlgo",
                           "mifid2ExecutionTrader",
                           "mifid2ExecutionAlgo",
                           "dontUseAutoPriceForHedge",
                           "isOmsContainer",
                           "discretionaryUpToLimitPrice",
                           "usePriceMgmtAlgo",
                           "duration",
                           "postToAts",
                           "autoCancelParent",
                           "advancedErrorOverride",
                           "manualOrderTime")],

                   if(contract$exchange == "IBKRATS")
                     order$minTradeQty,

                   if(order$orderType == "PEG BEST")
                     order[c("minCompeteSize",
                             "competeAgainstBestOffset")],

                   if(order$orderType == "PEG BEST" && order$competeAgainstBestOffset == Inf ||
                      order$orderType == "PEG MID")
                     order[c("midOffsetAtWhole",
                             "midOffsetAtHalf")],

                   if(self$serVersion >= MIN_SERVER_VER_CUSTOMER_ACCOUNT)
                     order$customerAccount,

                   if(self$serVersion >= MIN_SERVER_VER_PROFESSIONAL_CUSTOMER)
                     order$professionalCustomer,

                   if(self$serVersion >= MIN_SERVER_VER_RFQ_FIELDS)
                     order[c("externalUserId",
                             "manualOrderIndicator")])


      msg <- c(msg, id, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    cancelOrder= function(id, orderCancel) {

      msg <- c("4", "1", ### CANCEL_ORDER
               id,

               if(self$serVersion >= MIN_SERVER_VER_RFQ_FIELDS)
                 private$sanitize(orderCancel)
               else
                 orderCancel$manualOrderCancelTime)

      private$encodeMsg(msg)
    },

    reqOpenOrders= function() private$req_simple("5", "1"), ### REQ_OPEN_ORDERS

    reqAccountUpdates= function(subscribe, acctCode) {

      msg <- c("6", "2", ### REQ_ACCT_DATA
               private$sanitize(list(subscribe, acctCode)))

      private$encodeMsg(msg)
    },

    reqExecutions= function(reqId, filter) {

      msg <- c("7", "3", ### REQ_EXECUTIONS
               reqId,
               private$sanitize(filter))

      private$encodeMsg(msg)
    },

    reqIds= function()
      # Hardcode numIds = 1. It's deprecated and unused.
      private$req_simple("8", "1", "1"), ### REQ_IDS

    reqContractDetails= function(reqId, contract) {

      msg <- c("9", "8", ### REQ_CONTRACT_DATA
               reqId,
               private$sanitize(contract[1L:15L]),
               contract$issuerId)

      private$encodeMsg(msg)
    },

    reqMktDepth= function(tickerId, contract, numRows, isSmartDepth, mktDepthOptions=character()) {

      msg <- c("10", "5") ### REQ_MKT_DEPTH

      payload <- c(contract[1L:12L],
                   numRows,
                   isSmartDepth,
                   pack_tagvalue(mktDepthOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    cancelMktDepth= function(tickerId, isSmartDepth) {

      msg <- c("11", "1", ### CANCEL_MKT_DEPTH
               tickerId,
               private$sanitize(list(isSmartDepth)))

      private$encodeMsg(msg)
    },

    reqNewsBulletins= function(allMsgs) {

      msg <- c("12", "1", ### REQ_NEWS_BULLETINS
               private$sanitize(list(allMsgs)))

      private$encodeMsg(msg)
    },

    cancelNewsBulletins= function() private$req_simple("13", "1"), ### CANCEL_NEWS_BULLETINS

    setServerLogLevel= function(logLevel) private$req_simple("14", "1", logLevel), ### SET_SERVER_LOGLEVEL

    reqAutoOpenOrders= function(bAutoBind) {

      msg <- c("15", "1", ### REQ_AUTO_OPEN_ORDERS
             private$sanitize(list(bAutoBind)))

      private$encodeMsg(msg)
    },

    reqAllOpenOrders= function() private$req_simple("16", "1"), ### REQ_ALL_OPEN_ORDERS

    reqManagedAccts= function() private$req_simple("17", "1"), ### REQ_MANAGED_ACCTS

    requestFA= function(faDataType) private$req_simple("18", "1", map_enum2int("FaDataType", faDataType)), ### REQ_FA

    replaceFA= function(reqId, faDataType, xml) {

      msg <- c("19", "1", ### REPLACE_FA
               map_enum2int("FaDataType", faDataType),
               xml,
               reqId)

      private$encodeMsg(msg)
    },

    reqHistoricalData= function(tickerId, contract, endDateTime, durationStr, barSizeSetting, whatToShow, useRTH, formatDate, keepUpToDate, chartOptions=character()) {

      msg <- "20" ### REQ_HISTORICAL_DATA

      payload <- c(contract[1L:13L],
                   endDateTime,
                   barSizeSetting,
                   durationStr,
                   useRTH,
                   whatToShow,
                   formatDate,

                   if(contract$secType == "BAG")
                     c(length(contract$comboLegs),
                       do.call(c, lapply(contract$comboLegs, function(l) l[1L:4L]))),

                   keepUpToDate,
                   pack_tagvalue(chartOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    exerciseOptions= function(tickerId, contract, exerciseAction, exerciseQuantity,
                              account, override, manualOrderTime, customerAccount,
                              professionalCustomer) {

      msg <- c("21", "2") ### EXERCISE_OPTIONS

      payload <- c(contract[c(1L:8L, 10L:12L)],
                   exerciseAction,
                   exerciseQuantity,
                   account,
                   override,

                   if(self$serVersion >= MIN_SERVER_VER_MANUAL_ORDER_TIME_EXERCISE_OPTIONS)
                     manualOrderTime,

                   if(self$serVersion >= MIN_SERVER_VER_CUSTOMER_ACCOUNT)
                     customerAccount,

                   if(self$serVersion >= MIN_SERVER_VER_PROFESSIONAL_CUSTOMER)
                     professionalCustomer)

      msg <- c(msg, tickerId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    reqScannerSubscription= function(tickerId, subscription, scannerSubscriptionOptions=character(), scannerSubscriptionFilterOptions=character()) {

      msg <- "22" ### REQ_SCANNER_SUBSCRIPTION

      payload <- c(subscription[1L:21L],
                   pack_tagvalue(scannerSubscriptionFilterOptions, mode="string"),
                   pack_tagvalue(scannerSubscriptionOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    cancelScannerSubscription= function(tickerId) private$req_simple("23", "1", tickerId), ### CANCEL_SCANNER_SUBSCRIPTION

    reqScannerParameters= function() private$req_simple("24", "1"), ### REQ_SCANNER_PARAMETERS

    cancelHistoricalData= function(tickerId) private$req_simple("25", "1", tickerId), ### CANCEL_HISTORICAL_DATA

    reqCurrentTime= function() private$req_simple("49", "1"), ### REQ_CURRENT_TIME

    reqRealTimeBars= function(tickerId, contract, barSize, whatToShow, useRTH, realTimeBarsOptions=character()) {

      msg <- c("50", "3") ### REQ_REAL_TIME_BARS

      payload <- c(contract[1L:12L],
                   barSize,
                   whatToShow,
                   useRTH,
                   pack_tagvalue(realTimeBarsOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    cancelRealTimeBars= function(tickerId) private$req_simple("51", "1", tickerId), ### CANCEL_REAL_TIME_BARS

    reqFundamentalData= function(reqId, contract, reportType, fundamentalDataOptions=character()) {

      msg <- c("52", "2", ### REQ_FUNDAMENTAL_DATA
               reqId,
               private$sanitize(contract[c(1L:3L, 8L:11L)]),
               reportType,
               pack_tagvalue(fundamentalDataOptions, mode="string"))

      private$encodeMsg(msg)
    },

    cancelFundamentalData= function(reqId) private$req_simple("53", "1", reqId), ### CANCEL_FUNDAMENTAL_DATA

    calculateImpliedVolatility= function(reqId, contract, optionPrice, underPrice, miscOptions=character()) {

      msg <- c("54", "2") ### REQ_CALC_IMPLIED_VOLAT

      payload <- c(contract[1L:12L],
                   optionPrice,
                   underPrice,
                   pack_tagvalue(miscOptions, mode="string"))

      msg <- c(msg, reqId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    calculateOptionPrice= function(reqId, contract, volatility, underPrice, miscOptions=character()) {

      msg <- c("55", "2") ### REQ_CALC_OPTION_PRICE

      payload <- c(contract[1L:12L],
                   volatility,
                   underPrice,
                   pack_tagvalue(miscOptions, mode="string"))

      msg <- c(msg, reqId, private$sanitize(payload))

      private$encodeMsg(msg)
    },

    cancelCalculateImpliedVolatility= function(reqId) private$req_simple("56", "1", reqId), ### CANCEL_CALC_IMPLIED_VOLAT

    cancelCalculateOptionPrice= function(reqId) private$req_simple("57", "1", reqId), ### CANCEL_CALC_OPTION_PRICE

    reqGlobalCancel= function() private$req_simple("58", "1"), ### REQ_GLOBAL_CANCEL

    reqMarketDataType= function(marketDataType) private$req_simple("59", "1", marketDataType), ### REQ_MARKET_DATA_TYPE

    reqPositions= function() private$req_simple("61", "1"), ### REQ_POSITIONS

    reqAccountSummary= function(reqId, groupName, tags) private$req_simple("62", "1", reqId, groupName, tags), ### REQ_ACCOUNT_SUMMARY

    cancelAccountSummary= function(reqId) private$req_simple("63", "1", reqId), ### CANCEL_ACCOUNT_SUMMARY

    cancelPositions= function() private$req_simple("64", "1"), ### CANCEL_POSITIONS

    verifyRequest= function(apiName, apiVersion) {

      # WARN: Assume extraAuth = TRUE

      msg <- c("65", "1", ### VERIFY_REQUEST
               apiName,
               apiVersion)

      private$encodeMsg(msg)
    },

    verifyMessage= function(apiData) private$req_simple("66", "1", apiData), ### VERIFY_MESSAGE

    queryDisplayGroups= function(reqId) private$req_simple("67", "1", reqId), ### QUERY_DISPLAY_GROUPS

    subscribeToGroupEvents= function(reqId, groupId) private$req_simple("68", "1", reqId, groupId), ### SUBSCRIBE_TO_GROUP_EVENTS

    updateDisplayGroup= function(reqId, contractInfo) private$req_simple("69", "1", reqId, contractInfo), ### UPDATE_DISPLAY_GROUP

    unsubscribeFromGroupEvents= function(reqId) private$req_simple("70", "1", reqId), ### UNSUBSCRIBE_FROM_GROUP_EVENTS

    startApi= function(clientId, optionalCapabilities) private$req_simple("71", "2", clientId, optionalCapabilities), ### START_API

    verifyAndAuthRequest= function(apiName, apiVersion, opaqueIsvKey) {

      # WARN: Assume extraAuth = TRUE

      msg <- c("72", "1", ### VERIFY_AND_AUTH_REQUEST
               apiName,
               apiVersion,
               opaqueIsvKey)

      private$encodeMsg(msg)
    },

    verifyAndAuthMessage= function(apiData, xyzResponse) private$req_simple("73", "1", apiData, xyzResponse), ### VERIFY_AND_AUTH_MESSAGE

    reqPositionsMulti= function(reqId, account, modelCode) private$req_simple("74", "1", reqId, account, modelCode), ### REQ_POSITIONS_MULTI

    cancelPositionsMulti= function(reqId) private$req_simple("75", "1", reqId), ### CANCEL_POSITIONS_MULTI

    reqAccountUpdatesMulti= function(reqId, account, modelCode, ledgerAndNLV) {

      msg <- c("76", "1", ### REQ_ACCOUNT_UPDATES_MULTI
               reqId,
               account,
               modelCode,
               private$sanitize(list(ledgerAndNLV)))

      private$encodeMsg(msg)
    },

    cancelAccountUpdatesMulti= function(reqId) private$req_simple("77", "1", reqId), ### CANCEL_ACCOUNT_UPDATES_MULTI

    reqSecDefOptParams= function(reqId, underlyingSymbol, futFopExchange, underlyingSecType, underlyingConId) {

      msg <- c("78", ### REQ_SEC_DEF_OPT_PARAMS
               reqId,
               underlyingSymbol,
               futFopExchange,
               underlyingSecType,
               underlyingConId)

      private$encodeMsg(msg)
    },

    reqSoftDollarTiers= function(reqId) private$req_simple("79", reqId), ### REQ_SOFT_DOLLAR_TIERS

    reqFamilyCodes= function() private$req_simple("80"), ### REQ_FAMILY_CODES

    reqMatchingSymbols= function(reqId, pattern) private$req_simple("81", reqId, pattern), ### REQ_MATCHING_SYMBOLS

    reqMktDepthExchanges= function() private$req_simple("82"), ### REQ_MKT_DEPTH_EXCHANGES

    reqSmartComponents= function(reqId, bboExchange) private$req_simple("83", reqId, bboExchange), ### REQ_SMART_COMPONENTS

    reqNewsArticle= function(requestId, providerCode, articleId, newsArticleOptions=character()) {

      msg <- c("84", ### REQ_NEWS_ARTICLE
               requestId,
               providerCode,
               articleId,
               pack_tagvalue(newsArticleOptions, mode="string"))

      private$encodeMsg(msg)
    },

    reqNewsProviders= function() private$req_simple("85"), ### REQ_NEWS_PROVIDERS

    reqHistoricalNews= function(requestId, conId, providerCodes, startDateTime, endDateTime, totalResults, historicalNewsOptions=character()) {

      msg <- c("86", ### REQ_HISTORICAL_NEWS
               requestId,
               conId,
               providerCodes,
               startDateTime,
               endDateTime,
               totalResults,
               pack_tagvalue(historicalNewsOptions, mode="string"))

      private$encodeMsg(msg)
    },

    reqHeadTimestamp= function(tickerId, contract, whatToShow, useRTH, formatDate) {

      msg <- c("87", ### REQ_HEAD_TIMESTAMP
               tickerId,
               private$sanitize(c(contract[1L:13L], useRTH)),
               whatToShow,
               formatDate)

      private$encodeMsg(msg)
    },

    reqHistogramData= function(reqId, contract, useRTH, timePeriod) {

      msg <- c("88", ### REQ_HISTOGRAM_DATA
               reqId,
               private$sanitize(c(contract[1L:13L], useRTH)),
               timePeriod)

      private$encodeMsg(msg)
    },

    cancelHistogramData= function(reqId) private$req_simple("89", reqId), ### CANCEL_HISTOGRAM_DATA

    cancelHeadTimestamp= function(tickerId) private$req_simple("90", tickerId), ### CANCEL_HEAD_TIMESTAMP

    reqMarketRule= function(marketRuleId) private$req_simple("91", marketRuleId), ### REQ_MARKET_RULE

    reqPnL= function(reqId, account, modelCode) private$req_simple("92", reqId, account, modelCode), ### REQ_PNL

    cancelPnL= function(reqId) private$req_simple("93", reqId), ### CANCEL_PNL

    reqPnLSingle= function(reqId, account, modelCode, conId) private$req_simple("94", reqId, account, modelCode, conId), ### REQ_PNL_SINGLE

    cancelPnLSingle= function(reqId) private$req_simple("95", reqId), ### CANCEL_PNL_SINGLE

    reqHistoricalTicks= function(reqId, contract, startDateTime, endDateTime, numberOfTicks, whatToShow, useRth, ignoreSize, miscOptions=character()) {

      msg <- "96" ### REQ_HISTORICAL_TICKS

      payload <- c(contract[1L:13L],
                   startDateTime,
                   endDateTime,
                   numberOfTicks,
                   whatToShow,
                   useRth,
                   ignoreSize)

      msg <- c(msg, reqId, private$sanitize(payload), pack_tagvalue(miscOptions, mode="string"))

      private$encodeMsg(msg)
    },

    reqTickByTickData= function(reqId, contract, tickType, numberOfTicks, ignoreSize) {

      msg <- c("97", ### REQ_TICK_BY_TICK_DATA
               reqId,
               private$sanitize(contract[1L:12L]),
               tickType,
               numberOfTicks,
               private$sanitize(list(ignoreSize)))

      private$encodeMsg(msg)
    },

    cancelTickByTickData= function(reqId) private$req_simple("98", reqId), ### CANCEL_TICK_BY_TICK_DATA

    reqCompletedOrders= function(apiOnly) {

      msg <- c("99", ### REQ_COMPLETED_ORDERS
               private$sanitize(list(apiOnly)))

      private$encodeMsg(msg)
    },

    reqWshMetaData= function(reqId) private$req_simple("100", reqId), ### REQ_WSH_META_DATA

    cancelWshMetaData= function(reqId) private$req_simple("101", reqId), ### CANCEL_WSH_META_DATA

    reqWshEventData= function(reqId, wshEventData) {

      msg <- c("102", ### REQ_WSH_EVENT_DATA
               reqId,
               private$sanitize(wshEventData))

      private$encodeMsg(msg)
    },

    cancelWshEventData= function(reqId) private$req_simple("103", reqId), ### CANCEL_WSH_EVENT_DATA

    reqUserInfo= function(reqId) private$req_simple("104", reqId) ### REQ_USER_INFO
  )
)
