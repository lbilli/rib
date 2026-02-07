IBWrap <- R6Class("IBWrap",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  public= list(

    # Callbacks
    tickPrice= function(reqId, tickType, price, size, attrib) warning("default implementation"),

    tickSize= function(reqId, tickType, size) warning("default implementation"),

    tickOptionComputation= function(reqId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice) warning("default implementation"),

    tickGeneric= function(reqId, tickType, value) warning("default implementation"),

    tickString= function(reqId, tickType, value) warning("default implementation"),

    tickEFP= function(reqId, tickType, basisPoints, formattedBasisPoints, totalDividends, holdDays, futureLastTradeDate, dividendImpact, dividendsToLastTradeDate) warning("default implementation"),

    orderStatus= function(orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice) warning("default implementation"),

    openOrder= function(orderId, contract, order, orderState) warning("default implementation"),

    openOrderEnd= function() warning("default implementation"),

#    connectionClosed= function() warning("default implementation"),

    updateAccountValue= function(key, value, currency, accountName) warning("default implementation"),

    updatePortfolio= function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName) warning("default implementation"),

    updateAccountTime= function(timestamp) warning("default implementation"),

    accountDownloadEnd= function(accountName) warning("default implementation"),

    nextValidId= function(orderId) warning("default implementation"),

    contractDetails= function(reqId, contractDetails) warning("default implementation"),

    bondContractDetails= function(reqId, contractDetails) warning("default implementation"),

    contractDetailsEnd= function(reqId) warning("default implementation"),

    execDetails= function(reqId, contract, execution) warning("default implementation"),

    execDetailsEnd= function(reqId) warning("default implementation"),

    error= function(id, errorTime, errorCode, errorString, advancedOrderRejectJson) warning("default implementation"),

    updateMktDepth= function(reqId, position, operation, side, price, size) warning("default implementation"),

    updateMktDepthL2= function(reqId, position, marketMaker, operation, side, price, size, isSmartDepth) warning("default implementation"),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) warning("default implementation"),

    managedAccounts= function(accountsList) warning("default implementation"),

    receiveFA= function(faDataType, xml) warning("default implementation"),

    historicalData= function(reqId, bars) warning("default implementation"),

    scannerParameters= function(xml) warning("default implementation"),

    scannerData= function(reqId, data) warning("default implementation"),

    realtimeBar= function(reqId, time, open, high, low, close, volume, wap, count) warning("default implementation"),

    currentTime= function(time) warning("default implementation"),

    fundamentalData= function(reqId, data) warning("default implementation"),

    deltaNeutralValidation= function(reqId, deltaNeutralContract) warning("default implementation"),

    tickSnapshotEnd= function(reqId) warning("default implementation"),

    marketDataType= function(reqId, marketDataType) warning("default implementation"),

    commissionReport= function(commissionReport) warning("default implementation"),

    position= function(account, contract, position, avgCost) warning("default implementation"),

    positionEnd= function() warning("default implementation"),

    accountSummary= function(reqId, account, tag, value, currency) warning("default implementation"),

    accountSummaryEnd= function(reqId) warning("default implementation"),

    verifyMessageAPI= function(apiData) warning("default implementation"),

    verifyCompleted= function(isSuccessful, errorText) warning("default implementation"),

    displayGroupList= function(reqId, groups) warning("default implementation"),

    displayGroupUpdated= function(reqId, contractInfo) warning("default implementation"),

    verifyAndAuthMessageAPI= function(apiData, xyzChallange) warning("default implementation"),

    verifyAndAuthCompleted= function(isSuccessful, errorText) warning("default implementation"),

#    connectAck= function() warning("default implementation"),

    positionMulti= function(reqId, account, modelCode, contract, position, avgCost) warning("default implementation"),

    positionMultiEnd= function(reqId) warning("default implementation"),

    accountUpdateMulti= function(reqId, account, modelCode, key, value, currency) warning("default implementation"),

    accountUpdateMultiEnd= function(reqId) warning("default implementation"),

    securityDefinitionOptionalParameter= function(reqId, exchange, underlyingConId, tradingClass, multiplier, expirations, strikes) warning("default implementation"),

    securityDefinitionOptionalParameterEnd= function(reqId) warning("default implementation"),

    softDollarTiers= function(reqId, tiers) warning("default implementation"),

    familyCodes= function(familyCodes) warning("default implementation"),

    symbolSamples= function(reqId, contractDescriptions) warning("default implementation"),

    mktDepthExchanges= function(depthMktDataDescriptions) warning("default implementation"),

    tickNews= function(reqId, timestamp, providerCode, articleId, headline, extraData) warning("default implementation"),

    smartComponents= function(reqId, map) warning("default implementation"),

    tickReqParams= function(reqId, minTick, bboExchange, snapshotPermissions) warning("default implementation"),

    newsProviders= function(newsProviders) warning("default implementation"),

    newsArticle= function(reqId, articleType, articleText) warning("default implementation"),

    historicalNews= function(reqId, time, providerCode, articleId, headline) warning("default implementation"),

    historicalNewsEnd= function(reqId, hasMore) warning("default implementation"),

    headTimestamp= function(reqId, headTimestamp) warning("default implementation"),

    histogramData= function(reqId, data) warning("default implementation"),

    historicalDataUpdate= function(reqId, bar) warning("default implementation"),

    rerouteMktDataReq= function(reqId, conId, exchange) warning("default implementation"),

    rerouteMktDepthReq= function(reqId, conId, exchange) warning("default implementation"),

    marketRule= function(marketRuleId, priceIncrements) warning("default implementation"),

    pnl= function(reqId, dailyPnL, unrealizedPnL, realizedPnL) warning("default implementation"),

    pnlSingle= function(reqId, position, dailyPnL, unrealizedPnL, realizedPnL, value) warning("default implementation"),

    historicalTicks= function(reqId, ticks, done) warning("default implementation"),

    historicalTicksBidAsk= function(reqId, ticks, done) warning("default implementation"),

    historicalTicksLast= function(reqId, ticks, done) warning("default implementation"),

    tickByTickAllLast= function(reqId, tickType, time, price, size, attribs, exchange, specialConditions) warning("default implementation"),

    tickByTickBidAsk= function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs) warning("default implementation"),

    tickByTickMidPoint= function(reqId, time, price) warning("default implementation"),

    orderBound= function(permId, clientId, orderId) warning("default implementation"),

    completedOrder= function(contract, order, orderState) warning("default implementation"),

    completedOrdersEnd= function() warning("default implementation"),

    replaceFAEnd= function(reqId, data) warning("default implementation"),

    wshMetaData= function(reqId, data) warning("default implementation"),

    wshEventData= function(reqId, data) warning("default implementation"),

    historicalSchedule= function(reqId, startDateTime, endDateTime, timeZone, sessions) warning("default implementation"),

    userInfo= function(reqId, whiteBrandingId) warning("default implementation"),

    historicalDataEnd= function(reqId, startDate, endDate) warning("default implementation"),

    currentTimeInMillis= function(timeInMillis) warning("default implementation"),

    configResponseProtoBuf= function(configResponse) warning("default implementation"),

    updateConfigResponseProtoBuf= function(updateConfigResponse) warning("default implementation")
  )
)
