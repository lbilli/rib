IBWrapSimple <- R6::R6Class("IBWrapSimple",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  inherit= IBWrap,

  public= list(

    # Environment holding results
    context= NULL,

    initialize= function() self$context <- new.env(),

    # Override methods
    tickPrice=          function(tickerId, field, price, size, attrib)
                          cat("Price:", tickerId, field, price, size, unlist(attrib), "\n"),

    tickSize=           function(tickerId, field, size)
                          cat("Size:", tickerId, field, size, "\n"),

    tickOptionComputation= function(tickerId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice)
                             self$context$option <- list(tickerId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice),

    tickGeneric=        function(tickerId, tickType, value)
                          cat("Generic:", tickerId, tickType, value, "\n"),

    tickString=         function(tickerId, tickType, value)
                          cat("String:", tickerId, tickType, value, "\n"),

    orderStatus=        function(orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice)
                          cat("OrderStatus:", orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice, "\n"),

    openOrder=          function(orderId, contract, order, orderstate) {
                          self$context$order <- list(id=orderId, contract=contract, order=order, orderstate=orderstate)
                          cat("OpenOrder:", orderId, "\n")
                        },

    openOrderEnd=       function()
                          cat("OpenOrderEnd\n"),

    updateAccountValue= function(key, val, currency, accountName)
                          cat("AccountValue:", key, val, currency, accountName, "\n"),

    updatePortfolio=    function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName)
                          cat("updatePortfolio:", contract$symbol, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName, "\n"),

    updateAccountTime=  function(timeStamp)
                          cat("AccountTime:", timeStamp, "\n"),

    accountDownloadEnd= function(accountName)
                          cat("AccountDownloadEnd:", accountName, "\n"),

    nextValidId=        function(orderId)
                          self$context$nextId <- orderId,

    contractDetails=    function(reqId, contractDetails)
                          self$context$contract <- contractDetails,

    bondContractDetails=function(reqId, contractDetails)
                          self$context$bond <- contractDetails,

    contractDetailsEnd= function(reqId)
                          cat("ContractDetailsEnd:", reqId, "\n"),

    execDetails=        function(reqId, contract, execution) {
                          self$context$ex_contract <- contract
                          self$context$execution <- execution
                          cat("ExecDetails:", reqId, contract$symbol, execution$side, execution$shares, execution$price, "\n")
                        },

    execDetailsEnd=     function(reqId)
                          cat("ExecDetailsEnd:", reqId, "\n"),

    error=              function(id, errorCode, errorString)
                          cat("Error:", id, errorCode, errorString, "\n"),

    updateMktDepth=     function(id, position, operation, side, price, size)
                          cat("MktDepth:", id, position, operation, side, price, size, "\n"),

    updateMktDepthL2=   function(id, position, marketMaker, operation, side, price, size, isSmartDepth)
                          cat("MktDepthL2:", id, position, marketMaker, operation, side, price, size, isSmartDepth, "\n"),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) {
                          self$context$news <- newsMessage
                          cat("NewsBulletin:", msgId, msgType, originExch, "\n",
                                               newsMessage, "\n")
                        },

    managedAccounts=    function(accountsList)
                          self$context$accounts <- accountsList,

    receiveFA=          function(faDataType, xml) {
                          self$context$fa <- xml
                          cat("ReceiveFA:", faDataType, "\n")
                        },

    historicalData=     function(reqId, bar) {
                          self$context$historical <- bar
                          cat("Historical:", reqId, "Rows:", nrow(self$context$historical), "\n")
                        },

    scannerParameters=  function(xml) {
                          self$context$scannerParameters <- xml
                          cat("ScannerParameters: Received\n")
                        },

    scannerData=        function(reqId, rank, contractDetails, distance, benchmark, projection, legsStr) {
                          cat("Scanner:", reqId, length(rank), "\n")
                          for(i in seq_along(rank))
                            cat(" |", rank[i],
                                      paste0(contractDetails[[i]]$contract[c(1:3, 8)], collapse=" "),
                                      distance[i],
                                      benchmark[i],
                                      projection[i],
                                      legsStr[i], "|\n")
                        },

    realtimeBar=        function(reqId, time, open, high, low, close, volume, wap, count)
                          cat("realtimeBar:", reqId, time, open, high, low, close, volume, wap, count, "\n"),

    currentTime=        function(time)
                          cat("currentTime:", time, "\n"),

    fundamentalData=    function(reqId, data) {
                          self$context$fundamentalData <- data
                          cat("fundamentalData:", reqId, "\n")
                        },

    tickSnapshotEnd=    function(reqId)
                          cat("tickSnapshotEnd:", reqId, "\n"),

    marketDataType=     function(reqId, marketDataType)
                          cat("marketDataType:", reqId, map_int2enum("MarketData", marketDataType), "\n"),

    commissionReport=   function(commissionReport)
                          cat("CommissionReport:", unlist(commissionReport), "\n"),

    position=           function(account, contract, position, avgCost)
                          cat("Position:", account, contract$symbol, position, avgCost, "\n"),

    positionEnd=        function()
                          cat("PositionEnd\n"),

    accountSummary=     function(reqId, account, tag, value, currency)
                          cat("Account:", reqId, account, tag, value, currency, "\n"),

    accountSummaryEnd=  function(reqId)
                          cat("AccountEnd:", reqId, "\n"),

    displayGroupList=   function(reqId, groups)
                          cat("DisplayGroupList:", reqId, groups, "\n"),

    displayGroupUpdated= function(reqId, contractInfo)
                          cat("DisplayGroupUpdated:", reqId, contractInfo, "\n"),

    positionMulti=      function(reqId, account, modelCode, contract, position, avgCost)
                          cat("PositionMulti:", reqId, account, modelCode, contract$symbol, position, avgCost, "\n"),

    positionMultiEnd=   function(reqId)
                          cat("PositionMultiEnd:", reqId, "\n"),

    accountUpdateMulti= function(reqId, account, modelCode, key, value, currency)
                          cat("AccountUpdateMulti:", reqId, account, modelCode, key, value, currency, "\n"),

    accountUpdateMultiEnd= function(reqId)
                             cat("AccountUpdateMultiEnd:", reqId, "\n"),

    securityDefinitionOptionalParameter= function(reqId, exchange, underlyingConId, tradingClass, multiplier, expirations, strikes)
                                           cat("SecDefOptParams:", reqId, exchange, underlyingConId, tradingClass, multiplier, length(expirations), length(strikes), "\n"),

    securityDefinitionOptionalParameterEnd= function(reqId)
                                              cat("SecDefOptParamsEnd:", reqId, "\n"),

    softDollarTiers=    function(reqId, tiers) {
                          cat("softDollarTiers:", reqId, nrow(tiers), "\n")
                          print(tiers)
                        },

    familyCodes=        function(familyCodes) {
                          cat("familyCodes:", nrow(familyCodes), "\n")
                          print(familyCodes)
                        },

    symbolSamples=      function(reqId, contractDescriptions) {
                          cat("symbolSamples:", reqId, length(contractDescriptions), "\n")
                          for(cd in contractDescriptions)
                            cat("  ", cd$contract$conId, cd$contract$symbol, cd$contract$primaryExchange, cd$contract$currency, cd$derivativeSecTypes, "\n")
                        },

    mktDepthExchanges=  function(depthMktDataDescriptions) {
                          self$context$mktDepthExchanges <- depthMktDataDescriptions
                          cat("mktDepthExchanges:", nrow(depthMktDataDescriptions), "\n")
                        },

    tickNews=           function(tickerId, timeStamp, providerCode, articleId, headline, extraData)
                          cat("tickNews:", tickerId, timeStamp, providerCode, articleId, headline, extraData, "\n"),

    tickReqParams=      function(tickerId, minTick, bboExchange, snapshotPermissions)
                          cat("ReqParams:", tickerId, minTick, bboExchange, snapshotPermissions, "\n"),

    newsProviders=      function(newsProviders) {
                          cat("newsProviders:", nrow(newsProviders), "\n")
                          print(newsProviders)
                        },

    newsArticle=        function(requestId, articleType, articleText) {
                          self$context$newsarticle <- articleText
                          cat("newsArticle:", requestId, articleType, "\n")
                        },

    historicalNews=     function(requestId, time, providerCode, articleId, headline)
                          cat("historicalNews:", requestId, time, providerCode, articleId, headline, "\n"),

    historicalNewsEnd=  function(requestId, hasMore)
                          cat("historicalNewsEnd:", requestId, hasMore, "\n"),

    headTimestamp=      function(reqId, headTimestamp)
                          cat("headTimestamp:", reqId, headTimestamp, "\n"),

    histogramData=      function(reqId, data) {
                          self$context$histogram <- data
                          cat("histogramData:", reqId, nrow(data), "\n")
                        },

    historicalDataUpdate= function(reqId, bar)
                            cat("historicalDataUpdate:", reqId, unlist(bar), "\n"),

    marketRule=         function(marketRuleId, priceIncrements) {
                          cat("marketRule:", marketRuleId, nrow(priceIncrements), "\n")
                          print(priceIncrements)
                        },

    pnl=                function(reqId, dailyPnL, unrealizedPnL, realizedPnL)
                          cat("pnl:", reqId, dailyPnL, unrealizedPnL, realizedPnL, "\n"),

    pnlSingle=          function(reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value)
                          cat("pnlSingle:", reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value, "\n"),

    historicalTicks=    function(reqId, ticks, done) {
                          self$context$historicalTicks <- ticks
                          cat("historicalTicks:", reqId, done, "\n")
                        },

    historicalTicksBidAsk= function(reqId, ticks, done) {
                             self$context$historicalTicksBidAsk <- ticks
                             cat("historicalTicksBidAsk:", reqId, done, "\n")
                           },

    historicalTicksLast= function(reqId, ticks, done) {
                           self$context$historicalTicksLast <- ticks
                           cat("historicalTicksLast:", reqId, done, "\n")
                         },

    tickByTickAllLast=  function(reqId, tickType, time, price, size, attribs, exchange, specialConditions)
                          cat("tickByTickAllLast:", reqId, tickType, time, price, size, unlist(attribs), exchange, specialConditions, "\n"),

    tickByTickBidAsk=   function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs)
                          cat("tickByTickBidAsk:", reqId, time, bidPrice, askPrice, bidSize, askSize, unlist(attribs), "\n"),

    tickByTickMidPoint= function(reqId, time, midPoint)
                          cat("tickByTickMidPoint:", reqId, time, midPoint, "\n"),

    orderBound=         function(orderId, apiClientId, apiOrderId)
                          cat("orderBound:", orderId, apiClientId, apiOrderId, "\n"),

    completedOrder=     function(contract, order, orderState) {
                          self$context$completed <- list(contract=contract, order=order, orderstate=orderState)
                          cat("completedOrder\n")
                        },

    completedOrdersEnd= function()
                          cat("completedOrdersEnd\n"),

    replaceFAEnd=       function(reqId, text)
                          cat("replaceFAEnd:", reqId, text, "\n"),

    wshMetaData=        function(reqId, dataJson)
                          cat("wshMetaData:", reqId, dataJson, "\n"),

    wshEventData=       function(reqId, dataJson)
                          cat("wshEventData:", reqId, dataJson, "\n")
  )
)
