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

    encodeInit= function(msg) {

      stopifnot(length(msg) == 1L,
                is.character(msg),
                !is.na(msg))

      raw_msg <- charToRaw(msg) # Doesn't terminate with '\0'

      len <- length(raw_msg)

      stopifnot(raw_msg < as.raw(0x80L), # Only ASCII chars are allowed
                len <= MAX_MSG_LEN)

      header <- writeBin(len, raw(), size=HEADER_LEN, endian="big")

      # Write to socket
      writeBin(c(API_SIGN, header, raw_msg), private$socket)
    },

    #
    # Encode and send a message
    #
    # msgid: message id as integer
    # msg: protobuf message
    #
    encodeMsg= function(msgid, msg) {

      stopifnot(is.integer(msgid),
                missing(msg) || inherits(msg, "Message"))

      # Apply offset
      msgid <- msgid + PROTOBUF_MSG_ID

      raw_msg <- c(writeBin(msgid, raw(), size=RAWID_LEN, endian="big"),
                   if(!missing(msg)) RProtoBuf::serialize(msg, NULL))

      len <- length(raw_msg)

      stopifnot(len <= MAX_MSG_LEN)

      header <- writeBin(len, raw(), size=HEADER_LEN, endian="big")

      # Write to socket
      writeBin(c(header, raw_msg), private$socket)
    },

    readInit= function() {

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
      stopifnot(length(raw_msg) == len,
                raw_msg[len] == as.raw(0L),
                n == 2L)

      readBin(raw_msg, character(), n=n)
    },

    #
    # Read one message. BLOCKING
    #
    # Return a list(msgid(int), msg(message iterator or raw message))
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

      msgid <- readBin(private$socket, integer(), size=RAWID_LEN, endian="big")

      raw_msg <- readBin(private$socket, raw(), n=len - RAWID_LEN)

      list(msgid=msgid, msg=raw_msg)
    },

    # Convenience wrappers for simple payload requests
    req_int= function(msgid, reqId)
      private$encodeMsg(msgid,
                        RProtoBuf::new(IBProto.SingleInt32, value=reqId)),

    req_bool= function(msgid, value)
      private$encodeMsg(msgid,
                        if(value) RProtoBuf::new(IBProto.SingleBool, value=value)
                        else      RProtoBuf::new(IBProto.SingleBool)),

    req_intstring= function(msgid, reqId, data)
      private$encodeMsg(msgid,
                        RProtoBuf::new(IBProto.StringData, reqId=reqId, data=data)),

    encodeargs= function(msgid, descriptor) {

      args <- sapply(formalArgs(sys.function(-1L)), get, envir=parent.frame(), simplify=FALSE)

      stopifnot(names(args) %in% names(descriptor))

      msg <- maptopb(args, descriptor)

      private$encodeMsg(msgid, msg)
    }

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
      private$encodeInit(paste0("v", MIN_CLIENT_VER, "..", MAX_CLIENT_VER, connectOptions))

      # Server response
      res <- private$readInit()

      private$serverVersion   <- as.integer(res[1])
      private$serverTimestamp <- res[2]

      message("server version: ", private$serverVersion, " timestamp: ", private$serverTimestamp)

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

        if(missing(wrap))
          next

        # Decode message
        res <- decode(msg, private$serverVersion)

        # Dispatch callback
        if(!is.null(res))
          do.call(wrap[[res$fname]], res$fargs)
      }

      count
    },

    # ########################################################################
    #
    # Send requests to the server
    #
    # ########################################################################

    reqMktData= function(reqId, contract, genericTicks, snapshot, regulatorySnapshot=FALSE, mktDataOptions=character())
      private$encodeargs(1L, IBProto.MarketDataRequest), ### REQ_MKT_DATA

    cancelMktData= function(reqId) private$req_int(2L, reqId), ### CANCEL_MKT_DATA

    placeOrder= function(id, contract, order) {

      msgid <- 3L ### PLACE_ORDER

      c <- maptopb(contract, IBProto.Contract, "lastTradeDate")

      n <- length(order$orderComboLegs)

      stopifnot(n %in% c(0L, length(contract$comboLegs)))

      for(i in seq_len(n))
        c$comboLegs[[i]]$perLegPrice <- order$orderComboLegs[i]

      threestatebool <- c("routeMarketableToBbo",
                          "usePriceMgmtAlgo",
                          "seekPriceImprovement")

      attachedorders <- c("slOrderId",
                          "slOrderType",
                          "ptOrderId",
                          "ptOrderType")

      o <- maptopb(order, IBProto.Order, c("orderId",
                                           "rule80A",
                                           "auctionStrategy",
                                           "basisPoints",
                                           "basisPointsType",
                                           "orderComboLegs",
                                           "mifid2DecisionMaker",
                                           "mifid2DecisionAlgo",
                                           "mifid2ExecutionTrader",
                                           "mifid2ExecutionAlgo",
                                           "autoCancelDate",
                                           "filledQuantity",
                                           "refFuturesConId",
                                           "shareholder",
                                           "parentPermId",

                                           # Require separate handling
                                           "totalQuantity", # Decimal
                                           threestatebool,
                                           attachedorders))

      o$totalQuantity <- as.character(order$totalQuantity)

      for(n in threestatebool)
        if(!is.na(order[[n]]))
          o[[n]] <- order[[n]]

      if(self$serVersion < MIN_SERVER_VER_ADDITIONAL_ORDER_PARAMS_1)
        for(n in c("deactivate", "postOnly", "allowPreOpen", "ignoreOpenAuction"))
          if(o$has(n)) stop("Order parameter not supported: ", n)

      if(self$serVersion < MIN_SERVER_VER_ADDITIONAL_ORDER_PARAMS_2)
        for(n in c("routeMarketableToBbo", "seekPriceImprovement", "whatIfType"))
          if(o$has(n)) stop("Order parameter not supported: ", n)

      ao <- RProtoBuf::new(IBProto.AttachedOrders)

      for(n in attachedorders) {
        val <- order[[n]]

        if(is.na(val) || !nzchar(val))
          next

        if(self$serVersion < MIN_SERVER_VER_ATTACHED_ORDERS)
          stop("Attached order parameter not supported: ", n)
        else
          ao[[n]] <- val
      }

      msg <- RProtoBuf::new(IBProto.PlaceOrderRequest,
                            orderId=id,
                            contract=c,
                            order=o,
                            attachedOrders=ao)

      private$encodeMsg(msgid, msg)
    },

    cancelOrder= function(id, orderCancel)
      private$encodeMsg(4L, ### CANCEL_ORDER
                        RProtoBuf::new(IBProto.CancelOrderRequest,
                                       orderId=id,
                                       orderCancel=maptopb(orderCancel, IBProto.OrderCancel))),

    reqOpenOrders= function() private$encodeMsg(5L), ### REQ_OPEN_ORDERS

    reqAccountUpdates= function(subscribe, acctCode)
      private$encodeargs(6L, IBProto.AccountDataRequest),  ### REQ_ACCT_DATA

    reqExecutions= function(reqId, filter)
      private$encodeargs(7L, IBProto.ExecutionRequest), ### REQ_EXECUTIONS

    reqIds= function()
      # numIds is omitted as it's deprecated and unused
      private$encodeMsg(8L), ### REQ_IDS

    reqContractDetails= function(reqId, contract)
      private$encodeargs(9L, IBProto.ContractDataRequest), ### REQ_CONTRACT_DATA

    reqMktDepth= function(reqId, contract, numRows, isSmartDepth, mktDepthOptions=character())
      private$encodeargs(10L, IBProto.MarketDepthRequest), ### REQ_MKT_DEPTH

    cancelMktDepth= function(reqId, isSmartDepth)
      private$encodeargs(11L, IBProto.CancelMarketDepth), ### CANCEL_MKT_DEPTH

    reqNewsBulletins= function(allMsgs) private$req_bool(12L, allMsgs), ### REQ_NEWS_BULLETINS

    cancelNewsBulletins= function() private$encodeMsg(13L), ### CANCEL_NEWS_BULLETINS

    setServerLogLevel= function(logLevel) private$req_int(14L, logLevel), ### SET_SERVER_LOGLEVEL

    reqAutoOpenOrders= function(bAutoBind) private$req_bool(15L, bAutoBind), ### REQ_AUTO_OPEN_ORDERS

    reqAllOpenOrders= function() private$encodeMsg(16L), ### REQ_ALL_OPEN_ORDERS

    reqManagedAccts= function() private$encodeMsg(17L), ### REQ_MANAGED_ACCTS

    requestFA= function(faDataType) private$req_int(18L, map_enum2int("FaDataType", faDataType)), ### REQ_FA

    replaceFA= function(reqId, faDataType, xml)
      private$encodeMsg(19L, ### REPLACE_FA
                        RProtoBuf::new(IBProto.FAReplace,
                                       reqId=reqId,
                                       faDataType=map_enum2int("FaDataType", faDataType),
                                       xml=xml)),

    reqHistoricalData= function(reqId, contract, endDateTime, duration, barSizeSetting,
                                whatToShow, useRTH, formatDate, keepUpToDate,
                                chartOptions=character())
      private$encodeargs(20L, IBProto.HistoricalDataRequest), ### REQ_HISTORICAL_DATA

    exerciseOptions= function(reqId, contract, exerciseAction, exerciseQuantity,
                              account, override, manualOrderTime, customerAccount,
                              professionalCustomer)
      private$encodeargs(21L, IBProto.ExerciseOptionsRequest), ### EXERCISE_OPTIONS

    reqScannerSubscription= function(reqId, subscription, scannerSubscriptionOptions=character(), scannerSubscriptionFilterOptions=character()) {

      msgid <- 22L ### REQ_SCANNER_SUBSCRIPTION

      sub <- maptopb(c(subscription,
                       list(scannerSubscriptionOptions=scannerSubscriptionOptions,
                            scannerSubscriptionFilterOptions=scannerSubscriptionFilterOptions)),
                     IBProto.ScannerSubscription)

      msg <- RProtoBuf::new(IBProto.ScannerSubscriptionRequest,
                            reqId=reqId,
                            subscription=sub)

      private$encodeMsg(msgid, msg)
    },

    cancelScannerSubscription= function(reqId) private$req_int(23L, reqId), ### CANCEL_SCANNER_SUBSCRIPTION

    reqScannerParameters= function() private$encodeMsg(24L), ### REQ_SCANNER_PARAMETERS

    cancelHistoricalData= function(reqId) private$req_int(25L, reqId), ### CANCEL_HISTORICAL_DATA

    reqCurrentTime= function() private$encodeMsg(49L), ### REQ_CURRENT_TIME

    reqRealTimeBars= function(reqId, contract, barSize, whatToShow, useRTH, realTimeBarsOptions=character())
      private$encodeargs(50L, IBProto.RealTimeBarsRequest), ### REQ_REAL_TIME_BARS

    cancelRealTimeBars= function(reqId) private$req_int(51L, reqId), ### CANCEL_REAL_TIME_BARS

    reqFundamentalData= function(reqId, contract, reportType, fundamentalDataOptions=character())
      private$encodeargs(52L, ### REQ_FUNDAMENTAL_DATA
                         IBProto.FundamentalDataRequest),

    cancelFundamentalData= function(reqId) private$req_int(53L, reqId), ### CANCEL_FUNDAMENTAL_DATA

    calculateImpliedVolatility= function(reqId, contract, optionPrice, underPrice, miscOptions=character())
      private$encodeargs(54L, ### REQ_CALC_IMPLIED_VOLAT
                         IBProto.CalculateImpliedVolatilityRequest),

    calculateOptionPrice= function(reqId, contract, volatility, underPrice, miscOptions=character())
      private$encodeargs(55L, ### REQ_CALC_OPTION_PRICE
                         IBProto.CalculateOptionPriceRequest),

    cancelCalculateImpliedVolatility= function(reqId) private$req_int(56L, reqId), ### CANCEL_CALC_IMPLIED_VOLAT

    cancelCalculateOptionPrice= function(reqId) private$req_int(57L, reqId), ### CANCEL_CALC_OPTION_PRICE

    reqGlobalCancel= function(orderCancel)
      private$encodeMsg(58L, ### REQ_GLOBAL_CANCEL
                        RProtoBuf::new(IBProto.GlobalCancelRequest,
                                      orderCancel=maptopb(orderCancel, IBProto.OrderCancel))),

    reqMarketDataType= function(marketDataType) private$req_int(59L, marketDataType), ### REQ_MARKET_DATA_TYPE

    reqPositions= function() private$encodeMsg(61L), ### REQ_POSITIONS

    reqAccountSummary= function(reqId, group, tags)
      private$encodeargs(62L, IBProto.AccountSummaryRequest), ### REQ_ACCOUNT_SUMMARY

    cancelAccountSummary= function(reqId) private$req_int(63L, reqId), ### CANCEL_ACCOUNT_SUMMARY

    cancelPositions= function() private$encodeMsg(64L), ### CANCEL_POSITIONS

    verifyRequest= function(apiName, apiVersion)
      private$encodeargs(65L, IBProto.VerifyRequest), ### VERIFY_REQUEST

    verifyMessage= function(apiData)
      private$encodeMsg(66L, ### VERIFY_MESSAGE
                        RProtoBuf::new(IBProto.SingleString, value=apiData)),

    queryDisplayGroups= function(reqId) private$req_int(67L, reqId), ### QUERY_DISPLAY_GROUPS

    subscribeToGroupEvents= function(reqId, groupId)
      private$encodeargs(68L, IBProto.SubscribeToGroupEventsRequest), ### SUBSCRIBE_TO_GROUP_EVENTS

    updateDisplayGroup= function(reqId, contractInfo)
      private$req_intstring(69L, reqId, contractInfo), ### UPDATE_DISPLAY_GROUP

    unsubscribeFromGroupEvents= function(reqId) private$req_int(70L, reqId), ### UNSUBSCRIBE_FROM_GROUP_EVENTS

    startApi= function(clientId, optionalCapabilities)
      private$req_intstring(71L, clientId, optionalCapabilities), ### START_API

#     verifyAndAuthRequest= function(apiName, apiVersion, opaqueIsvKey) {
#
#       # WARN: Assume extraAuth = TRUE
#
#       msgid <- 72L ### VERIFY_AND_AUTH_REQUEST
#
#       msg <- c("1", apiName, apiVersion, opaqueIsvKey)
#
#       private$encodeMsg(msgid, msg)
#     },
#
#     verifyAndAuthMessage= function(apiData, xyzResponse) private$req_simple(73L, "1", apiData, xyzResponse), ### VERIFY_AND_AUTH_MESSAGE

    reqPositionsMulti= function(reqId, account, modelCode)
      private$encodeargs(74L, IBProto.PositionsMultiRequest), ### REQ_POSITIONS_MULTI

    cancelPositionsMulti= function(reqId) private$req_int(75L, reqId), ### CANCEL_POSITIONS_MULTI

    reqAccountUpdatesMulti= function(reqId, account, modelCode, ledgerAndNLV)
      private$encodeargs(76L, IBProto.AccountUpdatesMultiRequest), ### REQ_ACCOUNT_UPDATES_MULTI

    cancelAccountUpdatesMulti= function(reqId) private$req_int(77L, reqId),  ### CANCEL_ACCOUNT_UPDATES_MULTI

    reqSecDefOptParams= function(reqId, underlyingSymbol, futFopExchange, underlyingSecType, underlyingConId)
      private$encodeargs(78L, IBProto.SecDefOptParamsRequest), ### REQ_SEC_DEF_OPT_PARAMS

    reqSoftDollarTiers= function(reqId) private$req_int(79L, reqId), ### REQ_SOFT_DOLLAR_TIERS

    reqFamilyCodes= function() private$encodeMsg(80L), ### REQ_FAMILY_CODES

    reqMatchingSymbols= function(reqId, pattern)
      private$req_intstring(81L, reqId, pattern), ### REQ_MATCHING_SYMBOLS

    reqMktDepthExchanges= function() private$encodeMsg(82L), ### REQ_MKT_DEPTH_EXCHANGES

    reqSmartComponents= function(reqId, bboExchange)
      private$req_intstring(83L, reqId, bboExchange), ### REQ_SMART_COMPONENTS

    reqNewsArticle= function(reqId, providerCode, articleId, newsArticleOptions=character())
      private$encodeargs(84L, IBProto.NewsArticleRequest), ### REQ_NEWS_ARTICLE

    reqNewsProviders= function() private$encodeMsg(85L), ### REQ_NEWS_PROVIDERS

    reqHistoricalNews= function(reqId, conId, providerCodes, startDateTime, endDateTime, totalResults, historicalNewsOptions=character())
      private$encodeargs(86L, IBProto.HistoricalNewsRequest), ### REQ_HISTORICAL_NEWS

    reqHeadTimestamp= function(reqId, contract, whatToShow, useRTH, formatDate)
      private$encodeargs(87L, IBProto.HeadTimestampRequest), ### REQ_HEAD_TIMESTAMP

    reqHistogramData= function(reqId, contract, useRTH, timePeriod)
      private$encodeargs(88L, IBProto.HistogramDataRequest), ### REQ_HISTOGRAM_DATA

    cancelHistogramData= function(reqId) private$req_int(89L, reqId), ### CANCEL_HISTOGRAM_DATA

    cancelHeadTimestamp= function(reqId) private$req_int(90L, reqId), ### CANCEL_HEAD_TIMESTAMP

    reqMarketRule= function(marketRuleId) private$req_int(91L, marketRuleId), ### REQ_MARKET_RULE

    reqPnL= function(reqId, account, modelCode)
      private$encodeargs(92L, IBProto.PnLRequest), ### REQ_PNL

    cancelPnL= function(reqId) private$req_int(93L, reqId), ### CANCEL_PNL

    reqPnLSingle= function(reqId, account, modelCode, conId)
      private$encodeargs(94L, IBProto.PnLSingleRequest), ### REQ_PNL_SINGLE

    cancelPnLSingle= function(reqId) private$req_int(95L, reqId), ### CANCEL_PNL_SINGLE

    reqHistoricalTicks= function(reqId, contract, startDateTime, endDateTime, numberOfTicks, whatToShow, useRTH, ignoreSize, miscOptions=character())
      private$encodeargs(96L, IBProto.HistoricalTicksRequest), ### REQ_HISTORICAL_TICKS

    reqTickByTickData= function(reqId, contract, tickType, numberOfTicks, ignoreSize)
      private$encodeargs(97L, IBProto.TickByTickRequest), ### REQ_TICK_BY_TICK_DATA

    cancelTickByTickData= function(reqId) private$req_int(98L, reqId), ### CANCEL_TICK_BY_TICK_DATA

    reqCompletedOrders= function(apiOnly) private$req_bool(99L, apiOnly), ### REQ_COMPLETED_ORDERS

    reqWshMetaData= function(reqId) private$req_int(100L, reqId), ### REQ_WSH_META_DATA

    cancelWshMetaData= function(reqId) private$req_int(101L, reqId), ### CANCEL_WSH_META_DATA

    reqWshEventData= function(reqId, wshEventData) {

      msgid <- 102L ### REQ_WSH_EVENT_DATA

      msg <- maptopb(wshEventData, IBProto.WshEventDataRequest)

      msg$reqId <- reqId

      private$encodeMsg(msgid, msg)
    },

    cancelWshEventData= function(reqId) private$req_int(103L, reqId), ### CANCEL_WSH_EVENT_DATA

    reqUserInfo= function(reqId) private$req_int(104L, reqId), ### REQ_USER_INFO

    reqCurrentTimeInMillis= function() private$encodeMsg(105L), ### REQ_CURRENT_TIME_IN_MILLIS

    cancelContractData= function(reqId) private$req_int(106L, reqId), ### CANCEL_CONTRACT_DATA

    cancelHistoricalTick= function(reqId) private$req_int(107L, reqId) ### CANCEL_HISTORICAL_TICKS

  )
)
