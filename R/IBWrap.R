IBWrap <- R6::R6Class("IBWrap",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  public= list(

    # Callbacks
    tickPrice= function(tickerId, field, price, size, attrib) warning("Default implementation."),

    tickSize= function(tickerId, field, size) warning("Default implementation."),

    tickOptionComputation= function(tickerId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice) warning("Default implementation."),

    tickGeneric= function(tickerId, tickType, value) warning("Default implementation."),

    tickString= function(tickerId, tickType, value) warning("Default implementation."),

    tickEFP= function(tickerId, tickType, basisPoints, formattedBasisPoints, totalDividends, holdDays, futureLastTradeDate, dividendImpact, dividendsToLastTradeDate) warning("Default implementation."),

    orderStatus= function(orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice) warning("Default implementation."),

    openOrder= function(orderId, contract, order, orderstate) warning("Default implementation."),

    openOrderEnd= function() warning("Default implementation."),

#    connectionClosed= function() warning("Default implementation."),

    updateAccountValue= function(key, val, currency, accountName) warning("Default implementation."),

    updatePortfolio= function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName) warning("Default implementation."),

    updateAccountTime= function(timeStamp) warning("Default implementation."),

    accountDownloadEnd= function(accountName) warning("Default implementation."),

    nextValidId= function(orderId) warning("Default implementation."),

    contractDetails= function(reqId, contractDetails) warning("Default implementation."),

    bondContractDetails= function(reqId, contractDetails) warning("Default implementation."),

    contractDetailsEnd= function(reqId) warning("Default implementation."),

    execDetails= function(reqId, contract, execution) warning("Default implementation."),

    execDetailsEnd= function(reqId) warning("Default implementation."),

    error= function(id, errorCode, errorString) warning("Default implementation."),

    updateMktDepth= function(id, position, operation, side, price, size) warning("Default implementation."),

    updateMktDepthL2= function(id, position, marketMaker, operation, side, price, size, isSmartDepth) warning("Default implementation."),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) warning("Default implementation."),

    managedAccounts= function(accountsList) warning("Default implementation."),

    receiveFA= function(faDataType, xml) warning("Default implementation."),

    historicalData= function(reqId, bar) warning("Default implementation."),

    scannerParameters= function(xml) warning("Default implementation."),

    scannerData= function(reqId, rank, contractDetails, distance, benchmark, projection, legsStr) warning("Default implementation."),

    realtimeBar= function(reqId, time, open, high, low, close, volume, wap, count) warning("Default implementation."),

    currentTime= function(time) warning("Default implementation."),

    fundamentalData= function(reqId, data) warning("Default implementation."),

    deltaNeutralValidation= function(reqId, deltaNeutralContract) warning("Default implementation."),

    tickSnapshotEnd= function(reqId) warning("Default implementation."),

    marketDataType= function(reqId, marketDataType) warning("Default implementation."),

    commissionReport= function(commissionReport) warning("Default implementation."),

    position= function(account, contract, position, avgCost) warning("Default implementation."),

    positionEnd= function() warning("Default implementation."),

    accountSummary= function(reqId, account, tag, value, currency) warning("Default implementation."),

    accountSummaryEnd= function(reqId) warning("Default implementation."),

    verifyMessageAPI= function(apiData) warning("Default implementation."),

    verifyCompleted= function(isSuccessful, errorText) warning("Default implementation."),

    displayGroupList= function(reqId, groups) warning("Default implementation."),

    displayGroupUpdated= function(reqId, contractInfo) warning("Default implementation."),

    verifyAndAuthMessageAPI= function(apiData, xyzChallange) warning("Default implementation."),

    verifyAndAuthCompleted= function(isSuccessful, errorText) warning("Default implementation."),

#    connectAck= function() warning("Default implementation."),

    positionMulti= function(reqId, account, modelCode, contract, position, avgCost) warning("Default implementation."),

    positionMultiEnd= function(reqId) warning("Default implementation."),

    accountUpdateMulti= function(reqId, account, modelCode, key, value, currency) warning("Default implementation."),

    accountUpdateMultiEnd= function(reqId) warning("Default implementation."),

    securityDefinitionOptionalParameter= function(reqId, exchange, underlyingConId, tradingClass, multiplier, expirations, strikes) warning("Default implementation."),

    securityDefinitionOptionalParameterEnd= function(reqId) warning("Default implementation."),

    softDollarTiers= function(reqId, tiers) warning("Default implementation."),

    familyCodes= function(familyCodes) warning("Default implementation."),

    symbolSamples= function(reqId, contractDescriptions) warning("Default implementation."),

    mktDepthExchanges= function(depthMktDataDescriptions) warning("Default implementation."),

    tickNews= function(tickerId, timeStamp, providerCode, articleId, headline, extraData) warning("Default implementation."),

    smartComponents= function(reqId, theMap) warning("Default implementation."),

    tickReqParams= function(tickerId, minTick, bboExchange, snapshotPermissions) warning("Default implementation."),

    newsProviders= function(newsProviders) warning("Default implementation."),

    newsArticle= function(requestId, articleType, articleText) warning("Default implementation."),

    historicalNews= function(requestId, time, providerCode, articleId, headline) warning("Default implementation."),

    historicalNewsEnd= function(requestId, hasMore) warning("Default implementation."),

    headTimestamp= function(reqId, headTimestamp) warning("Default implementation."),

    histogramData= function(reqId, data) warning("Default implementation."),

    historicalDataUpdate= function(reqId, bar) warning("Default implementation."),

    rerouteMktDataReq= function(reqId, conid, exchange) warning("Default implementation."),

    rerouteMktDepthReq= function(reqId, conid, exchange) warning("Default implementation."),

    marketRule= function(marketRuleId, priceIncrements) warning("Default implementation."),

    pnl= function(reqId, dailyPnL, unrealizedPnL, realizedPnL) warning("Default implementation."),

    pnlSingle= function(reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value) warning("Default implementation."),

    historicalTicks= function(reqId, ticks, done) warning("Default implementation."),

    historicalTicksBidAsk= function(reqId, ticks, done) warning("Default implementation."),

    historicalTicksLast= function(reqId, ticks, done) warning("Default implementation."),

    tickByTickAllLast= function(reqId, tickType, time, price, size, attribs, exchange, specialConditions) warning("Default implementation."),

    tickByTickBidAsk= function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs) warning("Default implementation."),

    tickByTickMidPoint= function(reqId, time, midPoint) warning("Default implementation."),

    orderBound= function(orderId, apiClientId, apiOrderId) warning("Default implementation."),

    completedOrder= function(contract, order, orderState) warning("Default implementation."),

    completedOrdersEnd= function() warning("Default implementation."),

    replaceFAEnd= function(reqId, text) warning("Default implementation.")
  )
)
