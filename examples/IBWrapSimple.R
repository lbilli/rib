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
    tickPrice=          function(reqId, tickType, price, size, attrib)
                          cat("tickPrice:", reqId, tickType, price, size, unlist(attrib), "\n"),

    tickSize=           function(reqId, tickType, size)
                          cat("tickSize:", reqId, tickType, size, "\n"),

    tickOptionComputation= function(reqId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice) {
                             self$context$option <- list(tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice)
                             cat("tickOption:", reqId, tickType, "\n")
                           },

    tickGeneric=        function(reqId, tickType, value)
                          cat("tickGeneric:", reqId, tickType, value, "\n"),

    tickString=         function(reqId, tickType, value)
                          cat("tickString:", reqId, tickType, value, "\n"),

    orderStatus=        function(orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice)
                          cat("orderStatus:", orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice, "\n"),

    openOrder=          function(orderId, contract, order, orderState) {
                          self$context$order <- list(id=orderId, contract=contract, order=order, orderState=orderState)
                          cat("openOrder:", orderId, "\n")
                        },

    openOrderEnd=       function()
                          cat("openOrderEnd\n"),

    updateAccountValue= function(key, value, currency, accountName)
                          cat("accountValue:", key, value, currency, accountName, "\n"),

    updatePortfolio=    function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName)
                          cat("portfolio:", contract$symbol, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName, "\n"),

    updateAccountTime=  function(timestamp)
                          cat("accountTime:", timestamp, "\n"),

    accountDownloadEnd= function(accountName)
                          cat("accountDownloadEnd:", accountName, "\n"),

    nextValidId=        function(orderId)
                          self$context$nextId <- orderId,

    contractDetails=    function(reqId, contractDetails) {
                          self$context$contract <- contractDetails
                          cat("contractDetails:", reqId, contractDetails$contract$conId, "\n")
                         },

    bondContractDetails=function(reqId, contractDetails) {
                          self$context$bond <- contractDetails
                          cat("bondDetails:", reqId, contractDetails$contract$conId, "\n")
                        },

    contractDetailsEnd= function(reqId)
                          cat("contractDetailsEnd:", reqId, "\n"),

    execDetails=        function(reqId, contract, execution) {
                          self$context$ex_contract <- contract
                          self$context$execution <- execution
                          cat("execDetails:", reqId, contract$symbol, execution$time, execution$side, execution$shares, execution$price, "\n")
                        },

    execDetailsEnd=     function(reqId)
                          cat("execDetailsEnd:", reqId, "\n"),

    error=              function(id, errorTime, errorCode, errorString, advancedOrderRejectJson)
                          cat("error:", id, errorTime, errorCode, errorString, advancedOrderRejectJson, "\n"),

    updateMktDepth=     function(reqId, position, operation, side, price, size)
                          cat("mktDepth:", reqId, position, operation, side, price, size, "\n"),

    updateMktDepthL2=   function(reqId, position, marketMaker, operation, side, price, size, isSmartDepth)
                          cat("mktDepthL2:", reqId, position, marketMaker, operation, side, price, size, isSmartDepth, "\n"),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) {
                          self$context$news <- newsMessage
                          cat("newsBulletin:", msgId, msgType, originExch, "\n",
                                               newsMessage, "\n")
                        },

    managedAccounts=    function(accountsList)
                          self$context$accounts <- accountsList,

    receiveFA=          function(faDataType, xml) {
                          self$context$fa <- xml
                          cat("receiveFA:", faDataType, "\n")
                        },

    historicalData=     function(reqId, bars) {
                          self$context$historical <- bars
                          cat("historicalData:", reqId, "Rows:", length(bars), "\n")
                        },

    scannerParameters=  function(xml) {
                          self$context$scannerParameters <- xml
                          cat("scannerParameters: Received\n")
                        },

    scannerData=        function(reqId, data) {
                          self$context$scanner <- data
                          cat("scannerData:", reqId, length(data), "\n")
                        },

    realtimeBar=        function(reqId, time, open, high, low, close, volume, wap, count)
                          cat("realtimeBar:", reqId, time, open, high, low, close, volume, wap, count, "\n"),

    currentTime=        function(time) {
                          self$context$time <- time
                          cat("currentTime:", time, "\n")
                        },

    fundamentalData=    function(reqId, data) {
                          self$context$fundamentalData <- data
                          cat("fundamentalData:", reqId, "\n")
                        },

    tickSnapshotEnd=    function(reqId)
                          cat("tickSnapshotEnd:", reqId, "\n"),

    marketDataType=     function(reqId, marketDataType)
                          cat("marketDataType:", reqId, map_int2enum("MarketData", marketDataType), "\n"),

    commissionReport=   function(commissionReport)
                          cat("commissionReport:", unlist(commissionReport), "\n"),

    position=           function(account, contract, position, avgCost)
                          cat("position:", account, contract$symbol, position, avgCost, "\n"),

    positionEnd=        function()
                          cat("positionEnd\n"),

    accountSummary=     function(reqId, account, tag, value, currency)
                          cat("account:", reqId, account, tag, value, currency, "\n"),

    accountSummaryEnd=  function(reqId)
                          cat("accountEnd:", reqId, "\n"),

    displayGroupList=   function(reqId, groups)
                          cat("displayGroupList:", reqId, groups, "\n"),

    displayGroupUpdated= function(reqId, contractInfo)
                          cat("displayGroupUpdated:", reqId, contractInfo, "\n"),

    positionMulti=      function(reqId, account, modelCode, contract, position, avgCost)
                          cat("positionMulti:", reqId, account, modelCode, contract$symbol, position, avgCost, "\n"),

    positionMultiEnd=   function(reqId)
                          cat("positionMultiEnd:", reqId, "\n"),

    accountUpdateMulti= function(reqId, account, modelCode, key, value, currency)
                          cat("accountUpdateMulti:", reqId, account, modelCode, key, value, currency, "\n"),

    accountUpdateMultiEnd= function(reqId)
                             cat("accountUpdateMultiEnd:", reqId, "\n"),

    securityDefinitionOptionalParameter= function(reqId, exchange, underlyingConId, tradingClass, multiplier, expirations, strikes)
                                           cat("sdop:", reqId, exchange, underlyingConId, tradingClass, multiplier, length(expirations), length(strikes), "\n"),

    securityDefinitionOptionalParameterEnd= function(reqId)
                                              cat("sdopEnd:", reqId, "\n"),

    softDollarTiers=    function(reqId, tiers) {
                          self$context$softdollar <- tiers
                          cat("softDollarTiers:", reqId, length(tiers), "\n")
                        },

    familyCodes=        function(familyCodes) {
                          self$context$familycodes <- familyCodes
                          cat("familyCodes:", length(familyCodes), "\n")
                        },

    symbolSamples=      function(reqId, contractDescriptions) {
                          self$context$cds <- contractDescriptions
                          cat("symbolSamples:", reqId, length(contractDescriptions), "\n")
                        },

    mktDepthExchanges=  function(depthMktDataDescriptions) {
                          self$context$mktDepthExchanges <- depthMktDataDescriptions
                          cat("mktDepthExchanges:", length(depthMktDataDescriptions), "\n")
                        },

    tickNews=           function(reqId, timestamp, providerCode, articleId, headline, extraData)
                          cat("tickNews:", reqId, timestamp, providerCode, articleId, headline, extraData, "\n"),

    smartComponents=    function(reqId, map) {
                          self$context$smartcomponents <- map
                          cat("smartComponents:", reqId, length(map), "\n")
                        },

    tickReqParams=      function(reqId, minTick, bboExchange, snapshotPermissions)
                          cat("tickReqParams:", reqId, minTick, bboExchange, snapshotPermissions, "\n"),

    newsProviders=      function(newsProviders) {
                          self$context$newsproviders <- newsProviders
                          cat("newsProviders:", length(newsProviders), "\n")
                        },

    newsArticle=        function(reqId, articleType, articleText) {
                          self$context$newsarticle <- articleText
                          cat("newsArticle:", reqId, articleType, "\n")
                        },

    historicalNews=     function(reqId, time, providerCode, articleId, headline)
                          cat("historicalNews:", reqId, time, providerCode, articleId, headline, "\n"),

    historicalNewsEnd=  function(reqId, hasMore)
                          cat("historicalNewsEnd:", reqId, hasMore, "\n"),

    headTimestamp=      function(reqId, headTimestamp)
                          cat("headTimestamp:", reqId, headTimestamp, "\n"),

    histogramData=      function(reqId, data) {
                          self$context$histogram <- data
                          cat("histogramData:", reqId, length(data), "\n")
                        },

    historicalDataUpdate= function(reqId, bar)
                            cat("historicalDataUpdate:", reqId, unlist(bar), "\n"),

    rerouteMktDataReq=  function(reqId, conId, exchange)
                          cat("rerouteMktDataReq:", reqId, conId, exchange, "\n"),

    rerouteMktDepthReq= function(reqId, conId, exchange)
                          cat("rerouteMktDepthReq:", reqId, conId, exchange, "\n"),

    marketRule=         function(marketRuleId, priceIncrements) {
                          self$context$marketrule <- priceIncrements
                          cat("marketRule:", marketRuleId, length(priceIncrements), "\n")
                        },

    pnl=                function(reqId, dailyPnL, unrealizedPnL, realizedPnL)
                          cat("pnl:", reqId, dailyPnL, unrealizedPnL, realizedPnL, "\n"),

    pnlSingle=          function(reqId, position, dailyPnL, unrealizedPnL, realizedPnL, value)
                          cat("pnlSingle:", reqId, position, dailyPnL, unrealizedPnL, realizedPnL, value, "\n"),

    historicalTicks=    function(reqId, ticks, done) {
                          self$context$historicalTicks <- ticks
                          cat("historicalTicks:", reqId, done, length(ticks), "\n")
                        },

    historicalTicksBidAsk= function(reqId, ticks, done) {
                             self$context$historicalTicksBidAsk <- ticks
                             cat("historicalTicksBidAsk:", reqId, done, length(ticks), "\n")
                           },

    historicalTicksLast= function(reqId, ticks, done) {
                           self$context$historicalTicksLast <- ticks
                           cat("historicalTicksLast:", reqId, done, length(ticks), "\n")
                         },

    tickByTickAllLast=  function(reqId, tickType, time, price, size, attribs, exchange, specialConditions)
                          cat("tickByTickAllLast:", reqId, tickType, time, price, size, unlist(attribs), exchange, specialConditions, "\n"),

    tickByTickBidAsk=   function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs)
                          cat("tickByTickBidAsk:", reqId, time, bidPrice, askPrice, bidSize, askSize, unlist(attribs), "\n"),

    tickByTickMidPoint= function(reqId, time, price)
                          cat("tickByTickMidPoint:", reqId, time, price, "\n"),

    orderBound=         function(permId, clientId, orderId)
                          cat("orderBound:", permId, clientId, orderId, "\n"),

    completedOrder=     function(contract, order, orderState) {
                          self$context$completed <- list(contract=contract, order=order, orderState=orderState)
                          cat("completedOrder:", contract$symbol, orderState$status, "\n")
                        },

    completedOrdersEnd= function()
                          cat("completedOrdersEnd\n"),

    replaceFAEnd=       function(reqId, data)
                          cat("replaceFAEnd:", reqId, data, "\n"),

    wshMetaData=        function(reqId, data) {
                          self$context$wshmeta <- data
                          cat("wshMetaData:", reqId, "\n")
                        },

    wshEventData=       function(reqId, data) {
                          self$context$wshevents <- data
                          cat("wshEventData:", reqId, "\n")
                        },

    historicalSchedule= function(reqId, startDateTime, endDateTime, timeZone, sessions) {
                          self$context$schedule <- sessions
                          cat("historicalSchedule:", reqId, startDateTime, endDateTime, timeZone, "\n")
                        },

    userInfo=           function(reqId, whiteBrandingId)
                          cat("userInfo:", reqId, whiteBrandingId, "\n"),

    historicalDataEnd=  function(reqId, startDate, endDate)
                          cat("historicalDataEnd:", reqId, startDate, endDate, "\n"),

    currentTimeInMillis= function(timeInMillis) {
                           self$context$timemillis <- timeInMillis
                           cat("currentTimeInMillis:", timeInMillis, "\n")
                         },

    configResponseProtoBuf= function(configResponse) {
                              self$context$configpb <- configResponse
                              cat("configResponse:", configResponse$reqId, "\n")
                            },

    updateConfigResponseProtoBuf= function(updateConfigResponse) {
                                    self$context$updateconfigpb <- updateConfigResponse
                                    cat("updateConfigResponse:", updateConfigResponse$reqId, "\n")
                                  }
  )
)
