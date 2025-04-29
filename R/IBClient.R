IBClient <- R6Class("IBClient",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  private= list(

    socket=          NULL,  # Socket connection

    serverVersion=   NULL,  # Returned on connection
    serverTimestamp= NULL,  # Returned on connection

    id=              NULL,  # Client ID

    #
    # Finalizer
    #
    finalize= function() {

      self$disconnect()
    },

    #
    # Encode and send a message
    #
    # msgid: message id as integer, omit on first message
    # msg: character vector
    #
    encodeMsg= function(msgid, msg) {

      stopifnot(missing(msgid) && length(msg) == 1L || is.integer(msgid),
                is.character(msg),
                !is.na(msg))

      raw_msg <- if(missing(msgid))
                   charToRaw(msg) # Doesn't terminate with '\0'

                 else
                   c(if(self$serVersion >= MIN_SERVER_VER_PROTOBUF)
                       writeBin(msgid, raw(), size=RAWID_LEN, endian="big")
                     else
                       writeBin(as.character(msgid), raw()),

                     writeBin(msg, raw()))

      len <- length(raw_msg)

      stopifnot(raw_msg < as.raw(0x80L),   # Only ASCII chars are allowed
                len <= MAX_MSG_LEN)

      header <- writeBin(len, raw(), size=HEADER_LEN, endian="big")

      res <- if(missing(msgid)) c(API_SIGN, header, raw_msg)
             else               c(header, raw_msg)

      # Write to socket
      writeBin(res, private$socket)
    },

    encodeMsgPB= function(msgid, msg) {

      stopifnot(is.integer(msgid),
                inherits(msg, "Message"))

      # Apply offset
      msgid <- msgid + PROTOBUF_MSG_ID

      raw_msg <- c(writeBin(msgid, raw(), size=RAWID_LEN, endian="big"),
                   RProtoBuf::serialize(msg, NULL))

      len <- length(raw_msg)

      stopifnot(len <= MAX_MSG_LEN)

      header <- writeBin(len, raw(), size=HEADER_LEN, endian="big")

      # Write to socket
      writeBin(c(header, raw_msg), private$socket)
    },

    #
    # Read one message. BLOCKING
    #
    # Return a list(msgid(int), msg(message iterator or raw message))
    #
    readOneMsg= function(rawid=self$serVersion >= MIN_SERVER_VER_PROTOBUF) {

      # Read header and decode message length
      len <- readBin(private$socket, integer(), size=HEADER_LEN, endian="big")

      # Invalid socket
      if(length(len) == 0L)
        stop("lost connection")

      # Header consistency check
      stopifnot(len >  0L,
                len <= MAX_MSG_LEN)

      if(rawid) {
        msgid <- readBin(private$socket, integer(), size=RAWID_LEN, endian="big")

        len <- len - RAWID_LEN
      }

      raw_msg <- readBin(private$socket, raw(), n=len)

      msg <- if(!rawid || msgid <= PROTOBUF_MSG_ID) {
               # Count the fields
               n <- sum(raw_msg == as.raw(0L))

               # Consistency checks
               stopifnot(length(raw_msg) == len,                 # Entire message is read
                         n == 0L || raw_msg[len] == as.raw(0L))  # Last byte is 0x00

               make_iter(readBin(raw_msg, character(), n=n))
             }
             else
               raw_msg

      if(!rawid)
        msgid <- as.integer(msg$pop())

      list(msgid=msgid, msg=msg)
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
                  private$encodeMsg(msgid, as.character(c(...)))
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
      private$encodeMsg(msg=paste0("v", MIN_CLIENT_VER, "..", MAX_CLIENT_VER, connectOptions))

      # Server response
      res <- private$readOneMsg(FALSE)

      private$serverVersion   <- res$msgid
      private$serverTimestamp <- res$msg$pop()

      message("server version: ", private$serverVersion, " timestamp: ", private$serverTimestamp)

      if(res$msg$left() > 0L)
        warning("ignoring ", res$msg$left(), " fields")

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
    },

    disconnect= function() {

      if(!is.null(private$socket)) {

        close(private$socket)

        private$socket          <-
        private$serverVersion   <-
        private$serverTimestamp <-
        private$id              <- NULL
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
          res <- decode(msg, private$serverVersion)

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

      msgid <- 1L ### REQ_MKT_DATA

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

      msg <- c("11", tickerId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    cancelMktData= function(tickerId) private$req_simple(2L, "2", tickerId), ### CANCEL_MKT_DATA

    placeOrder= function(id, contract, order) {

      msgid <- 3L ### PLACE_ORDER

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
                           "faPercentage",
                           "modelCode",
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

                   order[c("customerAccount",
                           "professionalCustomer")],

                   if(self$serVersion < MIN_SERVER_VER_UNDO_RFQ_FIELDS)
                     c("", NA_integer_),

                   if(self$serVersion >= MIN_SERVER_VER_INCLUDE_OVERNIGHT)
                     order$includeOvernight,

                   if(self$serVersion >= MIN_SERVER_VER_CME_TAGGING_FIELDS)
                     order$manualOrderIndicator,

                   if(self$serVersion >= MIN_SERVER_VER_IMBALANCE_ONLY)
                     order$imbalanceOnly)


      msg <- c(id, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    cancelOrder= function(id, orderCancel) {

      msgid <- 4L ### CANCEL_ORDER

      msg <- c(if(self$serVersion < MIN_SERVER_VER_CME_TAGGING_FIELDS)
                 "1",

               id,
               orderCancel$manualOrderCancelTime,

               if(self$serVersion < MIN_SERVER_VER_UNDO_RFQ_FIELDS)
                 c("", "", ""),

               if(self$serVersion >= MIN_SERVER_VER_CME_TAGGING_FIELDS)
                 private$sanitize(orderCancel[c("extOperator",
                                                "manualOrderIndicator")]))

      private$encodeMsg(msgid, msg)
    },

    reqOpenOrders= function() private$req_simple(5L, "1"), ### REQ_OPEN_ORDERS

    reqAccountUpdates= function(subscribe, acctCode) {

      msgid <- 6L ### REQ_ACCT_DATA

      msg <- c("2", private$sanitize(list(subscribe, acctCode)))

      private$encodeMsg(msgid, msg)
    },

    reqExecutions= function(reqId, filter) {

      msgid <- 7L  ### REQ_EXECUTIONS

      if(self$serVersion >= MIN_SERVER_VER_PROTOBUF) {

        msg <-  RProtoBuf::new(IBProto.ExecutionRequest,
                               reqId=reqId,
                               executionFilter=maptopb(filter, IBProto.ExecutionFilter))

        private$encodeMsgPB(msgid, msg)
      }
      else {
        payload <- c(filter[1L:7L],

                     if(self$serVersion >= MIN_SERVER_VER_PARAMETRIZED_DAYS_OF_EXECUTIONS)
                       c(filter$lastNDays,

                         length(filter$specificDates),
                         filter$specificDates))

        msg <- c("3", reqId, private$sanitize(payload))

        private$encodeMsg(msgid, msg)
      }
    },

    reqIds= function()
      # Hardcode numIds = 1. It's deprecated and unused.
      private$req_simple(8L, "1", "1"), ### REQ_IDS

    reqContractDetails= function(reqId, contract) {

      msgid <- 9L ### REQ_CONTRACT_DATA

      msg <- c("8",
               reqId,
               private$sanitize(contract[1L:15L]),
               contract$issuerId)

      private$encodeMsg(msgid, msg)
    },

    reqMktDepth= function(tickerId, contract, numRows, isSmartDepth, mktDepthOptions=character()) {

      msgid <- 10L ### REQ_MKT_DEPTH

      payload <- c(contract[1L:12L],
                   numRows,
                   isSmartDepth,
                   pack_tagvalue(mktDepthOptions, mode="string"))

      msg <- c("5", tickerId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    cancelMktDepth= function(tickerId, isSmartDepth) {

      msgid <- 11L ### CANCEL_MKT_DEPTH

      msg <- c("1", tickerId, private$sanitize(list(isSmartDepth)))

      private$encodeMsg(msgid, msg)
    },

    reqNewsBulletins= function(allMsgs) {

      msgid <- 12L ### REQ_NEWS_BULLETINS

      msg <- c("1", private$sanitize(list(allMsgs)))

      private$encodeMsg(msgid, msg)
    },

    cancelNewsBulletins= function() private$req_simple(13L, "1"), ### CANCEL_NEWS_BULLETINS

    setServerLogLevel= function(logLevel) private$req_simple(14L, "1", logLevel), ### SET_SERVER_LOGLEVEL

    reqAutoOpenOrders= function(bAutoBind) {

      msgid <- 15L ### REQ_AUTO_OPEN_ORDERS

      msg <- c("1", private$sanitize(list(bAutoBind)))

      private$encodeMsg(msgid, msg)
    },

    reqAllOpenOrders= function() private$req_simple(16L, "1"), ### REQ_ALL_OPEN_ORDERS

    reqManagedAccts= function() private$req_simple(17L, "1"), ### REQ_MANAGED_ACCTS

    requestFA= function(faDataType) private$req_simple(18L, "1", map_enum2int("FaDataType", faDataType)), ### REQ_FA

    replaceFA= function(reqId, faDataType, xml) {

      msgid <- 19L ### REPLACE_FA

      msg <- c("1",
               map_enum2int("FaDataType", faDataType),
               xml,
               reqId)

      private$encodeMsg(msgid, msg)
    },

    reqHistoricalData= function(tickerId, contract, endDateTime, durationStr, barSizeSetting, whatToShow, useRTH, formatDate, keepUpToDate, chartOptions=character()) {

      msgid <- 20L ### REQ_HISTORICAL_DATA

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

      msg <- c(tickerId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    exerciseOptions= function(tickerId, contract, exerciseAction, exerciseQuantity,
                              account, override, manualOrderTime, customerAccount,
                              professionalCustomer) {

      msgid <- 21L ### EXERCISE_OPTIONS

      payload <- c(contract[c(1L:8L, 10L:12L)],
                   exerciseAction,
                   exerciseQuantity,
                   account,
                   override,
                   manualOrderTime,
                   customerAccount,
                   professionalCustomer)

      msg <- c("2", tickerId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    reqScannerSubscription= function(tickerId, subscription, scannerSubscriptionOptions=character(), scannerSubscriptionFilterOptions=character()) {

      msgid <- 22L ### REQ_SCANNER_SUBSCRIPTION

      payload <- c(subscription[1L:21L],
                   pack_tagvalue(scannerSubscriptionFilterOptions, mode="string"),
                   pack_tagvalue(scannerSubscriptionOptions, mode="string"))

      msg <- c(tickerId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    cancelScannerSubscription= function(tickerId) private$req_simple(23L, "1", tickerId), ### CANCEL_SCANNER_SUBSCRIPTION

    reqScannerParameters= function() private$req_simple(24L, "1"), ### REQ_SCANNER_PARAMETERS

    cancelHistoricalData= function(tickerId) private$req_simple(25L, "1", tickerId), ### CANCEL_HISTORICAL_DATA

    reqCurrentTime= function() private$req_simple(49L, "1"), ### REQ_CURRENT_TIME

    reqRealTimeBars= function(tickerId, contract, barSize, whatToShow, useRTH, realTimeBarsOptions=character()) {

      msgid <- 50L ### REQ_REAL_TIME_BARS

      payload <- c(contract[1L:12L],
                   barSize,
                   whatToShow,
                   useRTH,
                   pack_tagvalue(realTimeBarsOptions, mode="string"))

      msg <- c("3", tickerId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    cancelRealTimeBars= function(tickerId) private$req_simple(51L, "1", tickerId), ### CANCEL_REAL_TIME_BARS

    reqFundamentalData= function(reqId, contract, reportType, fundamentalDataOptions=character()) {

      msgid = 52L ### REQ_FUNDAMENTAL_DATA

      msg <- c("2",
               reqId,
               private$sanitize(contract[c(1L:3L, 8L:11L)]),
               reportType,
               pack_tagvalue(fundamentalDataOptions, mode="string"))

      private$encodeMsg(msgid, msg)
    },

    cancelFundamentalData= function(reqId) private$req_simple(53L, "1", reqId), ### CANCEL_FUNDAMENTAL_DATA

    calculateImpliedVolatility= function(reqId, contract, optionPrice, underPrice, miscOptions=character()) {

      msgid <- 54L ### REQ_CALC_IMPLIED_VOLAT

      payload <- c(contract[1L:12L],
                   optionPrice,
                   underPrice,
                   pack_tagvalue(miscOptions, mode="string"))

      msg <- c("2", reqId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    calculateOptionPrice= function(reqId, contract, volatility, underPrice, miscOptions=character()) {

      msgid <- 55L ### REQ_CALC_OPTION_PRICE

      payload <- c(contract[1L:12L],
                   volatility,
                   underPrice,
                   pack_tagvalue(miscOptions, mode="string"))

      msg <- c("2", reqId, private$sanitize(payload))

      private$encodeMsg(msgid, msg)
    },

    cancelCalculateImpliedVolatility= function(reqId) private$req_simple(56L, "1", reqId), ### CANCEL_CALC_IMPLIED_VOLAT

    cancelCalculateOptionPrice= function(reqId) private$req_simple(57L, "1", reqId), ### CANCEL_CALC_OPTION_PRICE

    reqGlobalCancel= function(orderCancel) {

      msgid <- 58L ### REQ_GLOBAL_CANCEL

      msg <- if(self$serVersion < MIN_SERVER_VER_CME_TAGGING_FIELDS)
               "1"
             else
               private$sanitize(orderCancel[c("extOperator",
                                              "manualOrderIndicator")])

      private$encodeMsg(msgid, msg)
    },

    reqMarketDataType= function(marketDataType) private$req_simple(59L, "1", marketDataType), ### REQ_MARKET_DATA_TYPE

    reqPositions= function() private$req_simple(61L, "1"), ### REQ_POSITIONS

    reqAccountSummary= function(reqId, groupName, tags) private$req_simple(62L, "1", reqId, groupName, tags), ### REQ_ACCOUNT_SUMMARY

    cancelAccountSummary= function(reqId) private$req_simple(63L, "1", reqId), ### CANCEL_ACCOUNT_SUMMARY

    cancelPositions= function() private$req_simple(64L, "1"), ### CANCEL_POSITIONS

    verifyRequest= function(apiName, apiVersion) {

      # WARN: Assume extraAuth = TRUE

      msgid <- 65L ### VERIFY_REQUEST

      msg <- c("1", apiName, apiVersion)

      private$encodeMsg(msgid, msg)
    },

    verifyMessage= function(apiData) private$req_simple(66L, "1", apiData), ### VERIFY_MESSAGE

    queryDisplayGroups= function(reqId) private$req_simple(67L, "1", reqId), ### QUERY_DISPLAY_GROUPS

    subscribeToGroupEvents= function(reqId, groupId) private$req_simple(68L, "1", reqId, groupId), ### SUBSCRIBE_TO_GROUP_EVENTS

    updateDisplayGroup= function(reqId, contractInfo) private$req_simple(69L, "1", reqId, contractInfo), ### UPDATE_DISPLAY_GROUP

    unsubscribeFromGroupEvents= function(reqId) private$req_simple(70L, "1", reqId), ### UNSUBSCRIBE_FROM_GROUP_EVENTS

    startApi= function(clientId, optionalCapabilities) private$req_simple(71L, "2", clientId, optionalCapabilities), ### START_API

    verifyAndAuthRequest= function(apiName, apiVersion, opaqueIsvKey) {

      # WARN: Assume extraAuth = TRUE

      msgid <- 72L ### VERIFY_AND_AUTH_REQUEST

      msg <- c("1", apiName, apiVersion, opaqueIsvKey)

      private$encodeMsg(msgid, msg)
    },

    verifyAndAuthMessage= function(apiData, xyzResponse) private$req_simple(73L, "1", apiData, xyzResponse), ### VERIFY_AND_AUTH_MESSAGE

    reqPositionsMulti= function(reqId, account, modelCode) private$req_simple(74L, "1", reqId, account, modelCode), ### REQ_POSITIONS_MULTI

    cancelPositionsMulti= function(reqId) private$req_simple(75L, "1", reqId), ### CANCEL_POSITIONS_MULTI

    reqAccountUpdatesMulti= function(reqId, account, modelCode, ledgerAndNLV) {

      msgid <- 76L ### REQ_ACCOUNT_UPDATES_MULTI

      msg <- c("1",
               reqId,
               account,
               modelCode,
               private$sanitize(list(ledgerAndNLV)))

      private$encodeMsg(msgid, msg)
    },

    cancelAccountUpdatesMulti= function(reqId) private$req_simple(77L, "1", reqId), ### CANCEL_ACCOUNT_UPDATES_MULTI

    reqSecDefOptParams= function(reqId, underlyingSymbol, futFopExchange, underlyingSecType, underlyingConId) {

      msgid <- 78L ### REQ_SEC_DEF_OPT_PARAMS

      msg <- c(reqId,
               underlyingSymbol,
               futFopExchange,
               underlyingSecType,
               underlyingConId)

      private$encodeMsg(msgid, msg)
    },

    reqSoftDollarTiers= function(reqId) private$req_simple(79L, reqId), ### REQ_SOFT_DOLLAR_TIERS

    reqFamilyCodes= function() private$req_simple(80L), ### REQ_FAMILY_CODES

    reqMatchingSymbols= function(reqId, pattern) private$req_simple(81L, reqId, pattern), ### REQ_MATCHING_SYMBOLS

    reqMktDepthExchanges= function() private$req_simple(82L), ### REQ_MKT_DEPTH_EXCHANGES

    reqSmartComponents= function(reqId, bboExchange) private$req_simple(83L, reqId, bboExchange), ### REQ_SMART_COMPONENTS

    reqNewsArticle= function(requestId, providerCode, articleId, newsArticleOptions=character()) {

      msgid <- 84L ### REQ_NEWS_ARTICLE

      msg <- c(requestId,
               providerCode,
               articleId,
               pack_tagvalue(newsArticleOptions, mode="string"))

      private$encodeMsg(msgid, msg)
    },

    reqNewsProviders= function() private$req_simple(85L), ### REQ_NEWS_PROVIDERS

    reqHistoricalNews= function(requestId, conId, providerCodes, startDateTime, endDateTime, totalResults, historicalNewsOptions=character()) {

      msgid <- 86L ### REQ_HISTORICAL_NEWS

      msg <- c(requestId,
               conId,
               providerCodes,
               startDateTime,
               endDateTime,
               totalResults,
               pack_tagvalue(historicalNewsOptions, mode="string"))

      private$encodeMsg(msgid, msg)
    },

    reqHeadTimestamp= function(tickerId, contract, whatToShow, useRTH, formatDate) {

      msgid <- 87L ### REQ_HEAD_TIMESTAMP

      msg <- c(tickerId,
               private$sanitize(c(contract[1L:13L], useRTH)),
               whatToShow,
               formatDate)

      private$encodeMsg(msgid, msg)
    },

    reqHistogramData= function(reqId, contract, useRTH, timePeriod) {

      msgid <- 88L ### REQ_HISTOGRAM_DATA

      msg <- c(reqId,
               private$sanitize(c(contract[1L:13L], useRTH)),
               timePeriod)

      private$encodeMsg(msgid, msg)
    },

    cancelHistogramData= function(reqId) private$req_simple(89L, reqId), ### CANCEL_HISTOGRAM_DATA

    cancelHeadTimestamp= function(tickerId) private$req_simple(90L, tickerId), ### CANCEL_HEAD_TIMESTAMP

    reqMarketRule= function(marketRuleId) private$req_simple(91L, marketRuleId), ### REQ_MARKET_RULE

    reqPnL= function(reqId, account, modelCode) private$req_simple(92L, reqId, account, modelCode), ### REQ_PNL

    cancelPnL= function(reqId) private$req_simple(93L, reqId), ### CANCEL_PNL

    reqPnLSingle= function(reqId, account, modelCode, conId) private$req_simple(94L, reqId, account, modelCode, conId), ### REQ_PNL_SINGLE

    cancelPnLSingle= function(reqId) private$req_simple(95L, reqId), ### CANCEL_PNL_SINGLE

    reqHistoricalTicks= function(reqId, contract, startDateTime, endDateTime, numberOfTicks, whatToShow, useRth, ignoreSize, miscOptions=character()) {

      msgid <- 96L ### REQ_HISTORICAL_TICKS

      payload <- c(contract[1L:13L],
                   startDateTime,
                   endDateTime,
                   numberOfTicks,
                   whatToShow,
                   useRth,
                   ignoreSize)

      msg <- c(reqId, private$sanitize(payload), pack_tagvalue(miscOptions, mode="string"))

      private$encodeMsg(msgid, msg)
    },

    reqTickByTickData= function(reqId, contract, tickType, numberOfTicks, ignoreSize) {

      msgid <- 97L ### REQ_TICK_BY_TICK_DATA

      msg <- c(reqId,
               private$sanitize(contract[1L:12L]),
               tickType,
               numberOfTicks,
               private$sanitize(list(ignoreSize)))

      private$encodeMsg(msgid, msg)
    },

    cancelTickByTickData= function(reqId) private$req_simple(98L, reqId), ### CANCEL_TICK_BY_TICK_DATA

    reqCompletedOrders= function(apiOnly) {

      msgid <- 99L ### REQ_COMPLETED_ORDERS

      private$encodeMsg(msgid, private$sanitize(list(apiOnly)))
    },

    reqWshMetaData= function(reqId) private$req_simple(100L, reqId), ### REQ_WSH_META_DATA

    cancelWshMetaData= function(reqId) private$req_simple(101L, reqId), ### CANCEL_WSH_META_DATA

    reqWshEventData= function(reqId, wshEventData) {

      msgid <- 102L ### REQ_WSH_EVENT_DATA

      if(is.na(wshEventData$conId) && nzchar(wshEventData$filter, keepNA=TRUE))
        wshEventData$conId <- 2147483647L

      msg <- c(reqId, private$sanitize(wshEventData))

      private$encodeMsg(msgid, msg)
    },

    cancelWshEventData= function(reqId) private$req_simple(103L, reqId), ### CANCEL_WSH_EVENT_DATA

    reqUserInfo= function(reqId) private$req_simple(104L, reqId), ### REQ_USER_INFO

    reqCurrentTimeInMillis= function() private$req_simple(105L) ### REQ_CURRENT_TIME_IN_MILLIS
  )
)
