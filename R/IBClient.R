IBClient <- R6::R6Class("IBClient",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  private= list(

    socket=          NULL,  # Socket connection

    serverVersion=   NULL,  # Returned on connection
    serverTimestamp= NULL,  # Returned on connection
    serverTZ=        NULL,  # Server Timezone

    Id=              NULL,  # Client ID

    wrap=            NULL,  # Callbacks wrapper

    decoder=         NULL,  # Decoder

    #
    # Finalizer
    #
    finalize= function() {

      self$disconnect()
    },

    #
    # Lookup message id and version
    #
    initMsg= function(name) {

      res <- map_outbound(name)

      if(is.na(res$version))
        res$id
      else
        c(res$id, res$version)
    },

    #
    # Encode and send a message
    #
    # msg: character vector
    #
    # Values are encoded into a \0 separated string
    # with a HEADER_LEN-byte header containing the length of the msg (excluding the header)
    #
    # Optional: API_SIGN can be prefixed to the message
    #
    encodeMsg= function(msg, api_sign=FALSE) {

      stopifnot(is.character(msg),
                !is.na(msg))

      raw_msg <- writeBin(msg, raw())

      len <- length(raw_msg)

      # Check that only ASCII chars are used
      # TODO: remove this?
      stopifnot(sum(nchar(msg, type="chars")) + length(msg) == len)

      if(len > MAX_MSG_LEN) {

        private$error(code="BAD_LENGTH")

        stop("Message too long.")
      }

      header <- writeBin(len, raw(), size=HEADER_LEN, endian="big")

      res <- if(api_sign) c(API_SIGN, header, raw_msg)
             else         c(header, raw_msg)

      # Write to socket
      stopifnot(self$isOpen)
      writeBin(res, private$socket)
    },

    #
    # Select on the connection socket
    #
    # timeout: in seconds; NULL for infinite
    #
    # Return TRUE if something is waiting to be read
    #
    wait= function(timeout) {

      # Wait for data to read
      stopifnot(self$isOpen)
      res <- socketSelect(list(private$socket), write=FALSE, timeout=timeout)

      # TODO: check execution time to catch dangling socket

      # TRUE if data available, FALSE if timed out
      res
    },

    countFields= function(r) {

      stopifnot(is.raw(r))

      sum(r==as.raw(0L))
    },

    #
    # Read and parse one message
    #
    # Return the fileds in a character vector
    #
    readOneMsg= function() {

      # Read header and decode message length
      len <- readBin(private$socket, integer(), size=HEADER_LEN, endian="big")

      # Return if nothing read
      if(length(len)==0L)
        return(character())

      # Header consistency check
      stopifnot(len >  0L,
                len <= MAX_MSG_LEN)

      raw_msg <- readBin(private$socket, raw(), n=len)

      # If message is incomplete, wait (up to 2 seconds) and read the rest
      while(length(raw_msg) < len) {

        res <- private$wait(timeout=2)

        # Stop on timeout
        if(!res) {
          private$error(code="BAD_MESSAGE")
          stop("Message incomplete")
        }

        raw_msg <- c(raw_msg, readBin(private$socket, raw(), n= len - length(raw_msg)))
      }

      n <- private$countFields(raw_msg)

      # Consistency checks
      # TODO: remove this
      stopifnot(raw_msg[length(raw_msg)] == as.raw(0L),  # Last byte is 0
                n > 1L)                                  # At least 2 fields (TODO: too strict?)

      res <- readBin(raw_msg, character(), n=n)

      # Consistency check
      # TODO: remove this
      stopifnot(sum(nchar(res, type="bytes")) + length(res) == len)

      res
    },

    #
    # Wait and read the messages from the server
    #
    # timeout:  seconds, NULL = infinite
    # dispatch: send the message to the decoder (if FALSE only one message is read and returned)
    #
    # Return number of messages processed (dispatch=TRUE) or one message content
    #
    waitAndRead= function(timeout, dispatch) {

      res <- private$wait(timeout=timeout)

      # Timeout
      if(!res)
        return(if(dispatch) 0L
               else         character())

      # Read all available messages and dispatch the callbacks
      if(dispatch) {

        count <- 0L

        while(length(msg <- private$readOneMsg()) > 0L) {

          count <- count + 1L

          # Decode inbound message
          res <- private$decoder$decode(msg)

          # Dispatch
          do.call(private$wrap[[res$fname]], res$fargs)
        }

        count
      }
      # Read one message and return it
      else
        private$readOneMsg()
    },

    # Dispatch wrap$error()
    error= function(id=NO_VALID_ID, code, msg="") {

      err <- map_error(code)

      private$wrap$error(id, Validator$i(err$code), paste(err$message, msg))
    },

    # Convert bool -> integer and check for NA
    # Return character vector
    sanitize= function(v) {

      stopifnot(is.list(v),
                !is.na(v))

      # Convert bool -> integer (0,1)
      if(length(idx <- which(vapply(v, is.logical, NA))) > 0L)
        v[idx] <- lapply(v[idx], as.integer)

      as.character(v)
    },

    # Convenience wrapper for simple payload requests
    req_simple= function(msgname, x=NULL)
                  private$encodeMsg(c(private$initMsg(msgname), x))
  ),

  active= list(

    serVersion=   function() private$serverVersion,

    serTimestamp= function() private$serverTimestamp,

    serTZ=        function() private$serverTZ,

    clientId=     function() private$Id,

    isOpen=       function() !is.null(private$socket) && isOpen(private$socket)
  ),

  public= list(

    initialize= function(wrap) {

      self$replaceWrap(wrap)
    },

    connect= function(host="localhost", port=4002L, clientId=1L, connectOptions="", optionalCapabilities="") {

      if(!is.null(private$socket)) {

        private$error(code="ALREADY_CONNECTED")

        stop("Already Connected.")
      }

      # Start connection to server
      private$socket <- socketConnection(host=host, port=port, open="r+b", blocking=FALSE)

      # Prefix " " to connectOptions, if not empty
      if(nzchar(connectOptions, keepNA=TRUE))
        connectOptions <- paste0(" ", connectOptions)

      #
      # Status: CONNECTING
      # Start handshake
      # Send "API\0" + HEADER + "v100..101[ connectOptions]\0"
      # (even in the case MIN_CLIENT_VER=MAX_CLIENT_VER, instead of "v100[ connectOptions]")
      #
      private$encodeMsg(paste0("v", MIN_CLIENT_VER, "..", MAX_CLIENT_VER, connectOptions), api_sign=TRUE)

      # Server response (wait up to 2 seconds)
      res <- private$waitAndRead(timeout=2, dispatch=FALSE)

      stopifnot(length(res)==2L)

cat("Server Version and Timestamp:", res, "\n")
      private$serverVersion   <- as.integer(res[1L])
      private$serverTimestamp <- from_IBtime(res[2L])
      private$serverTZ        <- attr(private$serTimestamp, "tzone")

      # TODO: Remove this, probably useless
      # Check server version
      if(private$serverVersion < MIN_CLIENT_VER || private$serverVersion > MAX_CLIENT_VER) {

        self$disconnect()

        private$error(code="UNSUPPORTED_VERSION", msg=private$serverVersion)

        stop("Unsupported server version. Disconnected.")
      }

      # Status: CONNECTED
      # startAPI
      self$startApi(clientId, optionalCapabilities)

      private$Id <- clientId

      # TODO
      # Verify that connection was successful: i.e. no other clients with same Id are connected

      # Dispatch connectAck()
      private$wrap$connectAck()

      # Instantiate Decoder
      private$decoder <- Decoder$new(private$serverVersion)

      # TODO
      # Dispatch IBWrap callbacks to handle server initial responses: Account and NextId
    },

    disconnect= function() {

      if(!is.null(private$socket)) {

        private$wrap$connectionClosed()

        close(private$socket)

        private$socket          <-
        private$serverVersion   <-
        private$serverTimestamp <-
        private$serverTZ        <-
        private$Id              <-
        private$decoder         <- NULL
      }
    },

    #
    # Replace callbacks wrapper on the fly
    #
    replaceWrap= function(wrap) {

      # Check if "wrap" is an IBWrap class
      # TODO Improve this
      stopifnot(exists("verifyAndAuthCompleted", wrap, mode="function", inherits=FALSE))

      private$wrap <- wrap
    },

    checkMsg= function(timeout=2)

      private$waitAndRead(timeout=timeout, dispatch=TRUE),

    #
    # Flush inbound queue
    #
    # Returns number of messages ignored
    #
    flush= function() {

      count <- 0L

      while(length(private$readOneMsg()) > 0L)
        count <- count + 1L

      count
    },

    # ########################################################################
    #
    # Methods to send messages to the server
    #
    # ########################################################################

    reqMktData= function(tickerId, contract, genericTicks="", snapshot=TRUE, regulatorySnaphsot=FALSE, mktDataOptions=character()) {

      msg <- private$initMsg("REQ_MKT_DATA")

      # Add payload (as list)
      payload <- contract[1L:12L]

      # ComboLegs
      if(contract$secType == "BAG") {

        payload <- c(payload, length(contract$comboLegs))

        for(combo in contract$comboLegs)
          payload <- c(payload, combo[1L:4L])
      }

      # DeltaNeutralContract
      deltaNeutralContract <- if(is.list(contract$deltaNeutralContract))
                                c(TRUE, contract$deltaNeutralContract)
                              else
                                FALSE


      payload <- c(payload, deltaNeutralContract, genericTicks, snapshot)

      if(self$serVersion >= MIN_SERVER_VER_REQ_SMART_COMPONENTS)
        payload <- c(payload, regulatorySnaphsot)

      payload <- c(payload, pack_tagvalue(mktDataOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelMktData= function(tickerId) private$req_simple("CANCEL_MKT_DATA", tickerId),

    placeOrder= function(id, contract, order) {

      msg <- private$initMsg("PLACE_ORDER")

      if(self$serVersion >= MIN_SERVER_VER_ORDER_CONTAINER)
        msg <- msg[1L]

      # Add payload
      payload <- contract[c(1L:12L, 14L, 15L)]

      payload <- c(payload, order[4L:9L])       # "action" through "tif"

      # Extended order fields
      payload <- c(payload, order[c("ocaGroup",
                                    "account",
                                    "openClose")],

                            map_enum2int("Origin", order$origin),

                            order[14L:22L])     # "orderRef" through "hidden"

      if(contract$secType == "BAG") {

        # Contract$comboLegs
        payload <- c(payload, length(contract$comboLegs))

        for(combo in contract$comboLegs)
          payload <- c(payload, combo)


        # Order$orderComboLegs
        payload <- c(payload,
                     length(order$orderComboLegs),
                     order$orderComboLegs)

        # Order$smartComboRoutingParams
        payload <- c(payload,
                     length(order$smartComboRoutingParams),
                     pack_tagvalue(order$smartComboRoutingParams, mode="unfold"))
      }

      payload <- c(payload, "",
                            order[c("discretionaryAmt",
                                    "goodAfterTime",
                                    "goodTillDate",
                                    "faGroup",
                                    "faMethod",
                                    "faPercentage",
                                    "faProfile",
                if(self$serVersion >= MIN_SERVER_VER_MODELS_SUPPORT) "modelCode",
                                    "shortSaleSlot",
                                    "designatedLocation",
                                    "exemptCode",
                                    "ocaType",
                                    "rule80A",
                                    "settlingFirm",
                                    "allOrNone",
                                    "minQty",
                                    "percentOffset",
                                    "eTradeOnly",
                                    "firmQuoteOnly",
                                    "nbboPriceCap")],

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
                                    "deltaNeutralAuxPrice")])

      if(nzchar(order$deltaNeutralOrderType, keepNA=TRUE))
        payload <- c(payload, order[58L:65L])

      payload <- c(payload, order[c("continuousUpdate",
                                    "referencePriceType",
                                    "trailStopPrice",
                                    "trailingPercent",
                                    "scaleInitLevelSize",
                                    "scaleSubsLevelSize",
                                    "scalePriceIncrement")])

      if(!is.na(order$scalePriceIncrement) && order$scalePriceIncrement > 0)
        payload <- c(payload, order[73L:79L])

      payload <- c(payload, order[c("scaleTable",
                                    "activeStartTime",
                                    "activeStopTime",
                                    "hedgeType")])
      if(nzchar(order$hedgeType, keepNA=TRUE))
        payload <- c(payload, order$hedgeParam)

      payload <- c(payload, order[c("optOutSmartRouting",
                                    "clearingAccount",
                                    "clearingIntent",
                                    "notHeld")])

      # DeltaNeutralContract
      deltaNeutralContract <- if(is.list(contract$deltaNeutralContract))
                                c(TRUE, contract$deltaNeutralContract)
                              else
                                FALSE


      payload <- c(payload, deltaNeutralContract, order$algoStrategy)

      if(nzchar(order$algoStrategy, keepNA=TRUE))
        payload <- c(payload, length(order$algoParams), pack_tagvalue(order$algoParams, mode="unfold"))

      payload <- c(payload, order[c("algoId",
                                    "whatIf")],

                            pack_tagvalue(order$orderMiscOptions, mode="string"),

                            order[c("solicited",
                                    "randomizeSize",
                                    "randomizePrice")])

      if(self$serVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK) {

        if(order$orderType == "PEG BENCH")
          payload <- c(payload, order[c("referenceContractId",
                                        "isPeggedChangeAmountDecrease",
                                        "peggedChangeAmount",
                                        "referenceChangeAmount",
                                        "referenceExchangeId")])

        payload <- c(payload, length(order$conditions))

        # Conditions
        if(length(order$conditions) > 0L) {

          for(cond in order$conditions)
            payload <- c(payload, map_enum2int("Condition", cond$type), cond[-1L])


            payload <- c(payload, order[c("conditionsIgnoreRth",
                                          "conditionsCancelOrder")])
        }

        payload <- c(payload, order[c("adjustedOrderType",
                                      "triggerPrice",
                                      "lmtPriceOffset",
                                      "adjustedStopPrice",
                                      "adjustedStopLimitPrice",
                                      "adjustedTrailingAmount",
                                      "adjustableTrailingUnit")])
      }

      if(self$serVersion >= MIN_SERVER_VER_EXT_OPERATOR)
        payload <- c(payload, order$extOperator)

      if(self$serVersion >= MIN_SERVER_VER_SOFT_DOLLAR_TIER)
        payload <- c(payload, order$softDollarTier[c("name", "val")])

      if(self$serVersion >= MIN_SERVER_VER_CASH_QTY)
        payload <- c(payload, order["cashQty"])      # To keep the name

      if(self$serVersion >= MIN_SERVER_VER_DECISION_MAKER)
        payload <- c(payload, order[c("mifid2DecisionMaker", "mifid2DecisionAlgo")])

      if(self$serVersion >= MIN_SERVER_VER_MIFID_EXECUTION)
        payload <- c(payload, order[c("mifid2ExecutionTrader", "mifid2ExecutionAlgo")])

      if(self$serVersion >= MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE)
        payload <- c(payload, order["dontUseAutoPriceForHedge"])

      if(self$serVersion >= MIN_SERVER_VER_ORDER_CONTAINER)
        payload <- c(payload, order["isOmsContainer"])

      if(self$serVersion >= MIN_SERVER_VER_D_PEG_ORDERS)
        payload <- c(payload, order["discretionaryUpToLimitPrice"])

      # TODO: remove this?
      # Check that NA's are only in allowed fields
      idx <- which(is.na(payload))
      stopifnot(names(payload)[idx] %in% c("lmtPrice", "auxPrice",
                                 "minQty",   "percentOffset",
                                 "nbboPriceCap", "startingPrice",
                                 "stockRefPrice", "delta",
                                 "stockRangeLower", "stockRangeUpper",
                                 "volatility", "volatilityType",
                                 "deltaNeutralAuxPrice", "referencePriceType",
                                 "trailStopPrice", "trailingPercent",
                                 "scaleInitLevelSize", "scaleSubsLevelSize",
                                 "scalePriceIncrement", "scalePriceAdjustValue",
                                 "scalePriceAdjustInterval", "scaleProfitOffset",
                                 "scaleInitPosition", "scaleInitFillQty", "cashQty",

                                 # The following ones are not set to "" in the API,
                                 # but left as .Machine$double.xmax
                                 "triggerPrice", "lmtPriceOffset", "adjustedStopPrice",
                                 "adjustedStopLimitPrice", "adjustedTrailingAmount"))
      # Convert NA -> ""
      payload[idx] <- ""


      msg <- c(msg, id, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelOrder= function(id) private$req_simple("CANCEL_ORDER", id),

    reqOpenOrders= function() private$req_simple("REQ_OPEN_ORDERS"),

    reqAccountUpdates= function(subscribe, acctCode) {

      msg <- private$initMsg("REQ_ACCT_DATA")

      # Add payload
      msg <- c(msg, private$sanitize(list(subscribe, acctCode)))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqExecutions= function(reqId, filter) {

      msg <- private$initMsg("REQ_EXECUTIONS")

      # Add payload
      msg <- c(msg, reqId, private$sanitize(filter))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqIds= function()
      # Hardcode numIds = 1. It's deprecated and unused.
      private$req_simple("REQ_IDS", 1L),

    reqContractDetails= function(reqId, contract) {

      msg <- private$initMsg("REQ_CONTRACT_DATA")

      # Add payload
      msg <- c(msg, reqId, private$sanitize(contract[1L:15L]))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqMktDepth= function(tickerId, contract, numRows, isSmartDepth, mktDepthOptions=character()) {

      msg <- private$initMsg("REQ_MKT_DEPTH")

      # Add payload
      payload <- c(contract[if(self$serVersion >= MIN_SERVER_VER_MKT_DEPTH_PRIM_EXCHANGE) 1L:12L
                            else c(1L:8L, 10:12L)],
                            numRows,
                            if(self$serVersion >= MIN_SERVER_VER_SMART_DEPTH)
                              isSmartDepth,
                            pack_tagvalue(mktDepthOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelMktDepth= function(tickerId, isSmartDepth) {

      msg <- private$initMsg("CANCEL_MKT_DEPTH")

      msg <- c(msg,
               tickerId,
               if(self$serVersion >= MIN_SERVER_VER_SMART_DEPTH)
                 private$sanitize(list(isSmartDepth)))

      private$encodeMsg(msg)
    },

    reqNewsBulletins= function(allMsgs) {

      msg <- private$initMsg("REQ_NEWS_BULLETINS")

      # Add payload
      msg <- c(msg, private$sanitize(list(allMsgs)))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelNewsBulletins= function() private$req_simple("CANCEL_NEWS_BULLETINS"),

    setServerLogLevel= function(logLevel) private$req_simple("SET_SERVER_LOGLEVEL", logLevel),

    reqAutoOpenOrders= function(bAutoBind) {

      msg <- private$initMsg("REQ_AUTO_OPEN_ORDERS")

      # Add payload
      msg <- c(msg, private$sanitize(list(bAutoBind)))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqAllOpenOrders= function() private$req_simple("REQ_ALL_OPEN_ORDERS"),

    reqManagedAccts= function() private$req_simple("REQ_MANAGED_ACCTS"),

    requestFA= function(pFaDataType) private$req_simple("REQ_FA", map_enum2int("faDataType", pFaDataType)),

    replaceFA= function(pFaDataType, cxml) private$req_simple("REPLACE_FA", c(map_enum2int("faDataType", pFaDataType), cxml)),

    reqHistoricalData= function(tickerId, contract, endDateTime, durationStr, barSizeSetting, whatToShow, useRTH, formatDate, keepUpToDate, chartOptions=character()) {

      msg <- private$initMsg("REQ_HISTORICAL_DATA")

      # Remove VERSION
      if(self$serVersion >= MIN_SERVER_VER_SYNT_REALTIME_BARS)
        msg <- msg[1L]

      # Add payload
      payload <- contract[1L:13L]

      payload <- c(payload, endDateTime, barSizeSetting, durationStr, useRTH, whatToShow, formatDate)

      # ComboLegs
      if(contract$secType == "BAG") {

        payload <- c(payload, length(contract$comboLegs))

        for(combo in contract$comboLegs)
          payload <- c(payload, combo[1L:4L])
      }

      if(self$serVersion >= MIN_SERVER_VER_SYNT_REALTIME_BARS)
        payload <- c(payload, keepUpToDate)


      payload <- c(payload, pack_tagvalue(chartOptions, mode="string"))

      msg <- c(msg, tickerId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    exerciseOptions= function(tickerId, contract, exerciseAction, exerciseQuantity, account, override) {

      msg <- private$initMsg("EXERCISE_OPTIONS")

      # Add payload
      payload <- contract[c(1L:8L, 10L:12L)]

      payload <- c(payload, exerciseAction, exerciseQuantity, account, override)

      msg <- c(msg, tickerId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqScannerSubscription= function(tickerId, subscription, scannerSubscriptionOptions=character(), scannerSubscriptionFilterOptions=character()) {

      msg <- private$initMsg("REQ_SCANNER_SUBSCRIPTION")

      if(self$serVersion >= MIN_SERVER_VER_SCANNER_GENERIC_OPTS)
        msg <- msg[1L]

      # Add payload
      payload <- c(subscription[1L:21L],
                   if(self$serVersion >= MIN_SERVER_VER_SCANNER_GENERIC_OPTS)
                     pack_tagvalue(scannerSubscriptionFilterOptions, mode="string"),
                   pack_tagvalue(scannerSubscriptionOptions, mode="string"))

      # TODO: remove this?
      # Check that NA's are only in allowed fields
      idx <- which(is.na(payload))
      stopifnot(names(payload)[idx] %in% c("numberOfRows", "abovePrice", "belowPrice",
                                           "aboveVolume", "marketCapAbove", "marketCapBelow",
                                           "couponRateAbove", "couponRateBelow",
                                           "excludeConvertible", "averageOptionVolumeAbove"))
      # Convert NA -> ""
      payload[idx] <- ""

      msg <- c(msg, tickerId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelScannerSubscription= function(tickerId) private$req_simple("CANCEL_SCANNER_SUBSCRIPTION", tickerId),

    reqScannerParameters= function() private$req_simple("REQ_SCANNER_PARAMETERS"),

    cancelHistoricalData= function(tickerId) private$req_simple("CANCEL_HISTORICAL_DATA", tickerId),

    reqCurrentTime= function() private$req_simple("REQ_CURRENT_TIME"),

    reqRealTimeBars= function(tickerId, contract, barSize, whatToShow, useRTH, realTimeBarsOptions=character()) {

      msg <- private$initMsg("REQ_REAL_TIME_BARS")

      payload <- c(contract[1L:12L],
                   barSize,
                   whatToShow,
                   useRTH,
                   pack_tagvalue(realTimeBarsOptions, mode="string"))


      # Add payload
      msg <- c(msg, tickerId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelRealTimeBars= function(tickerId) private$req_simple("CANCEL_REAL_TIME_BARS", tickerId),

    reqFundamentalData= function(reqId, contract, reportType, fundamentalDataOptions=character()) {

      msg <- private$initMsg("REQ_FUNDAMENTAL_DATA")

      # Add payload
      msg <- c(msg,
               reqId,
               private$sanitize(contract[c(1L:3L, 8L:11L)]),
               reportType,
               pack_tagvalue(fundamentalDataOptions, mode="string"))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelFundamentalData= function(reqId) private$req_simple("CANCEL_FUNDAMENTAL_DATA", reqId),

    calculateImpliedVolatility= function(reqId, contract, optionPrice, underPrice, miscOptions=character()) {

      msg <- private$initMsg("REQ_CALC_IMPLIED_VOLAT")

      # Add payload
      payload <- c(contract[1L:12L],
                   optionPrice,
                   underPrice,
                   pack_tagvalue(miscOptions, mode="string"))

      msg <- c(msg, reqId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    calculateOptionPrice= function(reqId, contract, volatility, underPrice, miscOptions=character()) {

      msg <- private$initMsg("REQ_CALC_OPTION_PRICE")

      # Add payload
      payload <- c(contract[1L:12L],
                   volatility,
                   underPrice,
                   pack_tagvalue(miscOptions, mode="string"))

      msg <- c(msg, reqId, private$sanitize(payload))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelCalculateImpliedVolatility= function(reqId) private$req_simple("CANCEL_CALC_IMPLIED_VOLAT", reqId),

    cancelCalculateOptionPrice= function(reqId) private$req_simple("CANCEL_CALC_OPTION_PRICE", reqId),

    reqGlobalCancel= function() private$req_simple("REQ_GLOBAL_CANCEL"),

    reqMarketDataType= function(marketDataType) private$req_simple("REQ_MARKET_DATA_TYPE", marketDataType),

    reqPositions= function() private$req_simple("REQ_POSITIONS"),

    reqAccountSummary= function(reqId, groupName, tags) private$req_simple("REQ_ACCOUNT_SUMMARY", c(reqId, groupName, tags)),

    cancelAccountSummary= function(reqId) private$req_simple("CANCEL_ACCOUNT_SUMMARY", reqId),

    cancelPositions= function() private$req_simple("CANCEL_POSITIONS"),

    verifyRequest= function(apiName, apiVersion) {

      msg <- private$initMsg("VERIFY_REQUEST")

      #
      # WARN: Assume extraAuth = TRUE
      #

      # Add payload
      msg <- c(msg, apiName, apiVersion)

      # Encode and send
      private$encodeMsg(msg)
    },

    verifyMessage= function(apiData) private$req_simple("VERIFY_MESSAGE", apiData),

    queryDisplayGroups= function(reqId) private$req_simple("QUERY_DISPLAY_GROUPS", reqId),

    subscribeToGroupEvents= function(reqId, groupId) private$req_simple("SUBSCRIBE_TO_GROUP_EVENTS", c(reqId, groupId)),

    updateDisplayGroup= function(reqId, contractInfo) private$req_simple("UPDATE_DISPLAY_GROUP", c(reqId, contractInfo)),

    unsubscribeFromGroupEvents= function(reqId) private$req_simple("UNSUBSCRIBE_FROM_GROUP_EVENTS", reqId),

    startApi= function(clientId, optionalCapabilities) private$req_simple("START_API", c(clientId, optionalCapabilities)),

    verifyAndAuthRequest= function(apiName, apiVersion, opaqueIsvKey) {

      msg <- private$initMsg("VERIFY_AND_AUTH_REQUEST")

      #
      # WARN: Assume extraAuth = TRUE
      #

      # Add payload
      msg <- c(msg, apiName, apiVersion, opaqueIsvKey)

      # Encode and send
      private$encodeMsg(msg)
    },

    verifyAndAuthMessage= function(apiData, xyzResponse) private$req_simple("VERIFY_AND_AUTH_MESSAGE", c(apiData, xyzResponse)),

    reqPositionsMulti= function(reqId, account, modelCode) private$req_simple("REQ_POSITIONS_MULTI", c(reqId, account, modelCode)),

    cancelPositionsMulti= function(reqId) private$req_simple("CANCEL_POSITIONS_MULTI", reqId),

    reqAccountUpdatesMulti= function(reqId, account, modelCode, ledgerAndNLV) {

      msg <- private$initMsg("REQ_ACCOUNT_UPDATES_MULTI")

      # Add payload
      msg <- c(msg, reqId, account, modelCode, private$sanitize(list(ledgerAndNLV)))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelAccountUpdatesMulti= function(reqId) private$req_simple("CANCEL_ACCOUNT_UPDATES_MULTI", reqId),

    reqSecDefOptParams= function(reqId, underlyingSymbol, futFopExchange, underlyingSecType, underlyingConId) {

      msg <- private$initMsg("REQ_SEC_DEF_OPT_PARAMS")

      #
      # WARN: since server version 104 the VERSION field is not used anymore
      # and msg = id instead of msg = c(id, version)
      #

      # Add payload
      msg <- c(msg, reqId, underlyingSymbol, futFopExchange, underlyingSecType, underlyingConId)

      # Encode and send
      private$encodeMsg(msg)
    },

    reqSoftDollarTiers= function(reqId) private$req_simple("REQ_SOFT_DOLLAR_TIERS", reqId),

    reqFamilyCodes= function() private$req_simple("REQ_FAMILY_CODES"),

    reqMatchingSymbols= function(reqId, pattern) private$req_simple("REQ_MATCHING_SYMBOLS", c(reqId, pattern)),

    reqMktDepthExchanges= function() private$req_simple("REQ_MKT_DEPTH_EXCHANGES"),

    reqSmartComponents= function(reqId, bboExchange) private$req_simple("REQ_SMART_COMPONENTS", c(reqId, bboExchange)),

    reqNewsArticle= function(requestId, providerCode, articleId, newsArticleOptions=character()) {

      msg <- private$initMsg("REQ_NEWS_ARTICLE")

      # Add payload
      msg <- c(msg, requestId, providerCode, articleId)

      if(self$serVersion >= MIN_SERVER_VER_NEWS_QUERY_ORIGINS)
        msg <- c(msg, pack_tagvalue(newsArticleOptions, mode="string"))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqNewsProviders= function() private$req_simple("REQ_NEWS_PROVIDERS"),

    reqHistoricalNews= function(requestId, conId, providerCodes, startDateTime, endDateTime, totalResults, historicalNewsOptions=character()) {

      msg <- private$initMsg("REQ_HISTORICAL_NEWS")

      # Add payload
      msg <- c(msg, requestId, conId, providerCodes, startDateTime, endDateTime, totalResults)

      if(self$serVersion >= MIN_SERVER_VER_NEWS_QUERY_ORIGINS)
        msg <- c(msg, pack_tagvalue(historicalNewsOptions, mode="string"))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqHeadTimestamp= function(tickerId, contract, whatToShow, useRTH, formatDate) {

      msg <- private$initMsg("REQ_HEAD_TIMESTAMP")

      # Add payload
      msg <- c(msg, tickerId, private$sanitize(c(contract[1L:13L], useRTH)), whatToShow, formatDate)

      # Encode and send
      private$encodeMsg(msg)
    },

    reqHistogramData= function(reqId, contract, useRTH, timePeriod) {

      msg <- private$initMsg("REQ_HISTOGRAM_DATA")

      # Add payload
      msg <- c(msg, reqId, private$sanitize(c(contract[1L:13L], useRTH)), timePeriod)

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelHistogramData= function(reqId) private$req_simple("CANCEL_HISTOGRAM_DATA", reqId),


    cancelHeadTimestamp= function(tickerId) private$req_simple("CANCEL_HEAD_TIMESTAMP", tickerId),

    reqMarketRule= function(marketRuleId) private$req_simple("REQ_MARKET_RULE", marketRuleId),

    reqPnL= function(reqId, account, modelCode) private$req_simple("REQ_PNL", c(reqId, account, modelCode)),

    cancelPnL= function(reqId) private$req_simple("CANCEL_PNL", reqId),

    reqPnLSingle= function(reqId, account, modelCode, conId) private$req_simple("REQ_PNL_SINGLE", c(reqId, account, modelCode, conId)),

    cancelPnLSingle= function(reqId) private$req_simple("CANCEL_PNL_SINGLE", reqId),

    reqHistoricalTicks= function(reqId, contract, startDateTime, endDateTime, numberOfTicks, whatToShow, useRth, ignoreSize, miscOptions=character()) {

      msg <- private$initMsg("REQ_HISTORICAL_TICKS")

      # Add payload
      payload <- c(contract[1L:13L],
                   startDateTime,
                   endDateTime,
                   numberOfTicks,
                   whatToShow,
                   useRth,
                   ignoreSize)

      msg <- c(msg, reqId, private$sanitize(payload), pack_tagvalue(miscOptions, mode="string"))

      # Encode and send
      private$encodeMsg(msg)
    },

    reqTickByTickData= function(reqId, contract, tickType, numberOfTicks, ignoreSize) {

      msg <- private$initMsg("REQ_TICK_BY_TICK_DATA")

      # Add payload
      msg <- c(msg, reqId, private$sanitize(contract[1L:12L]), tickType)

      if(self$serVersion >= MIN_SERVER_VER_TICK_BY_TICK_IGNORE_SIZE)
        msg <- c(msg, numberOfTicks, private$sanitize(list(ignoreSize)))

      # Encode and send
      private$encodeMsg(msg)
    },

    cancelTickByTickData= function(reqId) private$req_simple("CANCEL_TICK_BY_TICK_DATA", reqId)
  )
)
