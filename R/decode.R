#
# Decode message
#
decode <- function(msg, ver)
{
  stopifnot(is.list(msg))

  # Find handler
  handler <- process[[as.character(msg$msgid)]]

  if(is.null(handler)) {
    warning("unknown message id: ", msg$msgid)
    NULL
  }
  else
    # Call the appropriate handler
    handler(msg$msg, ver)
}

#
# Validate/convert args
#
validatepb <- function(fname, fargs)
{
  # Return callback name and argument list
  list(fname=fname, fargs=fargs)
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

  #
  # Messages using ProtoBuf have an offset applied
  #
  #  msgid -> msgid + PROTOBUF_MSG_ID = 200
  #

  # TICK_PRICE
  "201"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.TickPrice, msg)

    args <- splat(pb, price=NA_real_, size=NA_real_)

    validatepb("tickPrice", list(reqId=    args$reqId,
                                 tickType= ticktype(args$tickType),
                                 price=    args$price,
                                 size=     as.numeric(args$size),
                                 attrib=   unmask(args$attrMask,
                                                  c("canAutoExecute", "pastLimit", "preOpen"))))
  },

  # TICK_SIZE
  "202"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.TickString, msg)

    args <- splat(pb)

    validatepb("tickSize", list(reqId=    args$reqId,
                                tickType= ticktype(args$tickType),
                                size=     as.numeric(args$value)))
  },

  # ORDER_STATUS
  "203"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.OrderStatus, msg)

    args <- splat(pb, whyHeld="")

    args$filled <- as.numeric(args$filled)
    args$remaining <- as.numeric(args$remaining)

    validatepb("orderStatus", args)
  },

  # ERR_MSG
  "204"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.Error, msg)

    validatepb("error", splat(pb, errorCode=NA_integer_, advancedOrderRejectJson=""))
  },

  # OPEN_ORDER
  "205"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.OpenOrder, msg)

    args <- splat(pb)

    # Check orderId
    if(args$orderId != args$order$orderId)
      warning("orderId mismatch: ", args$orderId, args$order$orderId)

    # Transfer comboLeg prices
    for(i in seq_len(length(args$contract$comboLegs)))
      if(hasName(args$contract$comboLegs[[i]], "perLegPrice")) {
        args$order$orderComboLegs[i] <- args$contract$comboLegs[[i]]$perLegPrice
        args$contract$comboLegs[[i]]$perLegPrice <- NULL
      }

    if(any(is.na(args$order$orderComboLegs)))
      warning("perLegPrice: not all filled")

    # Conversions
    args$order$totalQuantity <- as.numeric(args$order$totalQuantity)
    args$order$filledQuantity <- as.numeric(args$order$filledQuantity)

    for(i in seq_len(length(args$orderState$orderAllocations)))
      args$orderState$orderAllocations[[i]][2L:6L] <-
        as.numeric(args$orderState$orderAllocations[[i]][2L:6L]) # position -> allowedAllocQty

    for(n in c("routeMarketableToBbo", "usePriceMgmtAlgo", "seekPriceImprovement")) {

      val <- args$order[[n]]

      if(val %in% c(0L, 1L))
        args$order[[n]] <- as.logical(val)
      else if(!is.na(val))
        warning("unexpected ", n, ": ", val)
    }

    validatepb("openOrder", args)
  },

  # ACCT_VALUE
  "206"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.AccountValue, msg)

    validatepb("updateAccountValue", splat(pb, currency=""))
  },

  # PORTFOLIO_VALUE
  "207"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.PortfolioValue, msg)

    args <- splat(pb, unrealizedPNL=0)

    args$position <- as.numeric(args$position)

    validatepb("updatePortfolio", args)
  },

  # ACCT_UPDATE_TIME
  "208"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleString, msg)

    validatepb("updateAccountTime", list(timestamp=pb$value))
  },

  # NEXT_VALID_ID
  "209"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("nextValidId", list(orderId=pb$value))
  },

  # CONTRACT_DATA
  "210"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.ContractData, msg)

    args <- splat(pb)

    args$contractDetails$contract <- args$contract

    fields <- c("minTick", "minSize", "sizeIncrement", "suggestedSizeIncrement", "minAlgoSize")
    args$contractDetails[fields] <- as.numeric(args$contractDetails[fields])

    args$contractDetails$fundDistributionPolicyIndicator <- funddist(args$contractDetails$fundDistributionPolicyIndicator)
    args$contractDetails$fundAssetType <- fundtype(args$contractDetails$fundAssetType)

    validatepb("contractDetails", args[c("reqId", "contractDetails")])
  },

  # EXECUTION_DATA
  "211"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.ExecutionDetails, msg)

    args <- splat(pb)

    args$execution$shares <- as.numeric(args$execution$shares)
    args$execution$cumQty <- as.numeric(args$execution$cumQty)
    args$execution$optExerciseOrLapseType <- optexercisetype(args$execution$optExerciseOrLapseType)

    validatepb("execDetails", args)
  },

  # MARKET_DEPTH
  "212"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.MarketDepth, msg)

    args <- as.list(pb$marketDepthData)

    args$size <- as.numeric(args$size)

    validatepb("updateMktDepth", c(reqId=pb$reqId, args[1L:5L]))
  },

  # MARKET_DEPTH_L2
  "213"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.MarketDepth, msg)

    args <- as.list(pb$marketDepthData)

    args$size <- as.numeric(args$size)

    # args are not in order, but they are named!
    validatepb("updateMktDepthL2", c(reqId=pb$reqId, args))
  },

  # NEWS_BULLETINS
  "214"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.NewsBulletin, msg)

    validatepb("updateNewsBulletin", splat(pb))
  },

  # MANAGED_ACCTS
  "215"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleString, msg)

    validatepb("managedAccounts", list(accountsList=pb$value))
  },

  # RECEIVE_FA
  "216"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("receiveFA",
               list(faDataType=map_int2enum("FaDataType", pb$reqId), xml=pb$data))
  },

  # HISTORICAL_DATA
  "217"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalData, msg)

    args <- as.list(pb)

    args$bars <- lapply(args$bars, function(b) {

                                     res <- splat(b)

                                     res$volume <- as.numeric(res$volume)
                                     res$wap <- as.numeric(res$wap)

                                     res
                                   })

    list(fname="historicalData", fargs=args)
  },

  # BOND_CONTRACT_DATA
  "218"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.ContractData, msg)

    args <- splat(pb)

    args$contractDetails$contract <- args$contract

    fields <- c("minTick", "minSize", "sizeIncrement", "suggestedSizeIncrement", "minAlgoSize")
    args$contractDetails[fields] <- as.numeric(args$contractDetails[fields])

    validatepb("bondContractDetails", args[c("reqId", "contractDetails")])
  },

  # SCANNER_PARAMETERS
  "219"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleString, msg)

    validatepb("scannerParameters", list(xml=pb$value))
  },

  # SCANNER_DATA
  "220"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.ScannerData, msg)

    args <- as.list(pb)

    args$data <- lapply(args$data, splat, distance="",
                                          benchmark="",
                                          projection="",
                                          comboKey="")

    validatepb("scannerData", args)
  },

  # TICK_OPTION_COMPUTATION
  "221"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.TickOptionComputation, msg)

    args <- splat(pb)

    args$tickType <- ticktype(args$tickType)

    idx <- c("impliedVol", "optPrice", "pvDividend", "undPrice")
    args[idx[args[idx] == -1]] <- NA_real_

    idx <- c("delta", "gamma", "vega", "theta")
    args[idx[args[idx] == -2]] <- NA_real_

    validatepb("tickOptionComputation", args)
  },

  # TICK_GENERIC
  "245"= function(msg, ver) {

     pb <- RProtoBuf::read(IBProto.TickGeneric, msg)

     args <- splat(pb)

     args$tickType <- ticktype(args$tickType)

     validatepb("tickGeneric", args)
  },

  # TICK_STRING
  "246"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.TickString, msg)

    args <- splat(pb)

    args$tickType <- ticktype(args$tickType)

    validatepb("tickString", args)
  },

  # CURRENT_TIME
  "249"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt64, msg)

    validatepb("currentTime", list(time=as.integer(pb$value)))
  },

  # REAL_TIME_BARS
  "250"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.RealTimeBarTick, msg)

    args <- splat(pb)

    args$time <- as.integer(args$time)

    args$volume <- as.numeric(args$volume)
    args$wap <- as.numeric(args$wap)

    validatepb("realtimeBar", args)
  },

  # FUNDAMENTAL_DATA
  "251"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("fundamentalData", splat(pb))
  },

  # CONTRACT_DATA_END
  "252"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("contractDetailsEnd", list(reqId=pb$value))
  },

  # OPEN_ORDER_END
  "253"= function(msg, ver) validatepb("openOrderEnd", list()),

  # ACCT_DOWNLOAD_END
  "254"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleString, msg)

    validatepb("accountDownloadEnd", list(accountName=pb$value))
  },

  # EXECUTION_DATA_END
  "255"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("execDetailsEnd", list(reqId=pb$value))
  },

  # TICK_SNAPSHOT_END
  "257"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("tickSnapshotEnd", list(reqId=pb$value))
  },

  # MARKET_DATA_TYPE
  "258"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.MarketDataType, msg)

    validatepb("marketDataType", splat(pb))
  },

  # COMMISSION_REPORT
  "259"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.CommissionReport, msg)

    args <- splat(pb, realizedPNL=NA_real_,
                      yield=NA_real_,
                      yieldRedemptionDate=NA_integer_)

    args$yieldRedemptionDate <- as.integer(args$yieldRedemptionDate)

    validatepb("commissionReport", list(commissionReport=args))
  },

  # POSITION_DATA
  "261"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.Position, msg)

    args <- splat(pb)

    args$position <- as.numeric(args$position)

    validatepb("position", args)
  },

  # POSITION_END
  "262"= function(msg, ver) validatepb("positionEnd", list()),

  # ACCOUNT_SUMMARY
  "263"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.AccountSummary, msg)

    validatepb("accountSummary", splat(pb, currency=""))
  },

  # ACCOUNT_SUMMARY_END
  "264"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("accountSummaryEnd", list(reqId=pb$value))
  },

  # VERIFY_MESSAGE_API
  "265"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleString, msg)

    validatepb("verifyMessageAPI", list(apiData=pb$value))
  },

  # VERIFY_COMPLETED
  "266"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.VerifyCompleted, msg)

    validatepb("verifyCompleted", splat(pb))
  },

  # DISPLAY_GROUP_LIST
  "267"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("displayGroupList", list(reqId=pb$reqId, groups=pb$data))
  },

  # DISPLAY_GROUP_UPDATED
  "268"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("displayGroupUpdated", list(reqId=pb$reqId, contractInfo=pb$data))
  },

  # POSITION_MULTI
  "271"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.PositionMulti, msg)

    args <- splat(pb, modelCode="")

    args$position <- as.numeric(args$position)

    validatepb("positionMulti", args)
  },

  # POSITION_MULTI_END
  "272"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("positionMultiEnd", list(reqId=pb$value))
  },

  # ACCOUNT_UPDATE_MULTI
  "273"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.AccountUpdateMulti, msg)

    validatepb("accountUpdateMulti", splat(pb, modelCode="", currency=""))
  },

  # ACCOUNT_UPDATE_MULTI_END
  "274"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("accountUpdateMultiEnd", list(reqId=pb$value))
  },

  # SECURITY_DEFINITION_OPTION_PARAMETER
  "275"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SecDefOptParameter, msg)

    validatepb("securityDefinitionOptionalParameter", splat(pb))
  },

  # SECURITY_DEFINITION_OPTION_PARAMETER_END
  "276"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt32, msg)

    validatepb("securityDefinitionOptionalParameterEnd", list(reqId=pb$value))
  },

  # SOFT_DOLLAR_TIERS
  "277"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SoftDollarTiers, msg)

    validatepb("softDollarTiers", splat(pb, tiers=list()))
  },

  # FAMILY_CODES
  "278"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.FamilyCodes, msg)

    args <- as.list(pb)
    args$familyCodes <- lapply(args$familyCodes, splat, familyCode="")

    validatepb("familyCodes", args)
  },

  # SYMBOL_SAMPLES
  "279"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SymbolSamples, msg)

    args <- as.list(pb)
    args$contractDescriptions <- lapply(args$contractDescriptions, splat, derivativeSecTypes=character())

    validatepb("symbolSamples", args)
  },

  # MKT_DEPTH_EXCHANGES
  "280"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.MarketDepthExchanges, msg)

    args <- as.list(pb)
    args$depthMktDataDescriptions <- lapply(args$depthMktDataDescriptions, splat, listingExch="", aggGroup=NA_integer_)

    validatepb("mktDepthExchanges", args)
  },


  # TICK_REQ_PARAMS
  "281"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.TickReqParams, msg)

    args <- splat(pb, bboExchange="")

    args$minTick <- as.numeric(args$minTick)

    validatepb("tickReqParams", args)
  },

  # SMART_COMPONENTS
  "282"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SmartComponents, msg)

    args <- as.list(pb)

    args$map <- lapply(args$map, as.list)

    list(fname="smartComponents", fargs=args)
  },

  # NEWS_ARTICLE
  "283"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.NewsArticle, msg)

    validatepb("newsArticle", splat(pb))
  },

  # TICK_NEWS
  "284"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.TickNews, msg)

    validatepb("tickNews", splat(pb))
  },

  # NEWS_PROVIDERS
  "285"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.NewsProviders, msg)

    validatepb("newsProviders", splat(pb, newsProviders=list()))
  },

  # HISTORICAL_NEWS
  "286"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalNews, msg)

    validatepb("historicalNews", splat(pb))
  },

  # HISTORICAL_NEWS_END
  "287"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalNewsEnd, msg)

    validatepb("historicalNewsEnd", splat(pb))
  },

  # HEAD_TIMESTAMP
  "288"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("headTimestamp", list(reqId=pb$reqId, headTimestamp=pb$data))
  },

   # HISTOGRAM_DATA
  "289"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistogramData, msg)

    args <- as.list(pb)

    args$data <- lapply(args$data, function(b) {

                                     res <- as.list(b)

                                     res$size <- as.numeric(res$size)

                                     res
                                   })

     list(fname="histogramData", fargs=args)
  },

  # HISTORICAL_DATA_UPDATE
  "290"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalDataUpdate, msg)

    args <- splat(pb)

    args$bar$volume <- as.numeric(args$bar$volume)
    args$bar$wap <- as.numeric(args$bar$wap)

    validatepb("historicalDataUpdate", args)
  },

  # REROUTE_MKT_DATA_REQ
  "291"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.Reroute, msg)

    validatepb("rerouteMktDataReq", splat(pb))
  },

  # REROUTE_MKT_DEPTH_REQ
  "292"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.Reroute, msg)

    validatepb("rerouteMktDepthReq", splat(pb))
  },

  # MARKET_RULE
  "293"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.MarketRule, msg)

    validatepb("marketRule", splat(pb))
  },

  # PNL
  "294"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.PnL, msg)

    validatepb("pnl", splat(pb))
  },

  # PNL_SINGLE
  "295"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.PnLSingle, msg)

    args <- splat(pb, dailyPnL=NA_real_, unrealizedPnL=NA_real_, realizedPnL=NA_real_)

    args$position <- as.numeric(args$position)

    validatepb("pnlSingle", args)
  },

  # HISTORICAL_TICKS
  "296"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalTicks, msg)

    args <- as.list(pb)

    args$ticks <- lapply(args$ticks, function(t) {

                                       res <- as.list(t)

                                       res$time <- as.integer(res$time)
                                       res$size <- as.numeric(res$size)

                                       res
                                     })

    list(fname="historicalTicks", fargs=args)
  },

  # HISTORICAL_TICKS_BID_ASK
  "297"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalTicksBidAsk, msg)

    args <- as.list(pb)

    args$ticks <- lapply(args$ticks, function(t) {

                                       res <- as.list(t)

                                       res$time <- as.integer(res$time)
                                       res$attribs <- as.list(res$attribs)
                                       res$bidSize <- as.numeric(res$bidSize)
                                       res$askSize <- as.numeric(res$askSize)

                                       res
                                     })

    list(fname="historicalTicksBidAsk", fargs=args)
  },

  # HISTORICAL_TICKS_LAST
  "298"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalTicksLast, msg)

    args <- as.list(pb)

    args$ticks <- lapply(args$ticks, function(t) {

                                       res <- as.list(t)

                                       res$time <- as.integer(res$time)
                                       res$attribs <- as.list(res$attribs)
                                       res$size <- as.numeric(res$size)

                                       res
                                     })

    list(fname="historicalTicksLast", fargs=args)
  },

  # TICK_BY_TICK
  "299"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.TickByTickData, msg)

    if(pb$tickType %in% c(1L, 2L)){

      args <- splat(pb$tickLast, exchange="", specialConditions="")

      args$time <- as.integer(args$time)
      args$size <- as.numeric(args$size)

      validatepb("tickByTickAllLast", c(list(reqId=pb$reqId, tickType=pb$tickType), args))
    }
    else if(pb$tickType == 3L) {

      args <- splat(pb$tickBidAsk)

      args$time <- as.integer(args$time)
      args$bidSize <- as.numeric(args$bidSize)
      args$askSize <- as.numeric(args$askSize)

      validatepb("tickByTickBidAsk", c(reqId=pb$reqId, args))
    }
    else if(pb$tickType == 4L) {

      args <- splat(pb$tickMidPoint)

      args$time <- as.integer(args$time)

      validatepb("tickByTickMidPoint", c(reqId=pb$reqId, args[c("time", "price")]))
    }
    else {
      warning("TICK_BY_TICK: unknown TickType ", pb$tickType)
      NULL
    }
  },

  # ORDER_BOUND
  "300"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.OrderBound, msg)

    validatepb("orderBound", splat(pb))
  },

  # COMPLETED_ORDER
  "301"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.CompletedOrder, msg)

    args <- splat(pb)

    # TODO:
    # - Transfer comboleg price
    # - Decimal Conversion

    # Transfer comboLeg prices
    for(i in seq_len(length(args$contract$comboLegs)))
      if(hasName(args$contract$comboLegs[[i]], "perLegPrice")) {
        args$order$orderComboLegs[i] <- args$contract$comboLegs[[i]]$perLegPrice
        args$contract$comboLegs[[i]]$perLegPrice <- NULL
      }

    if(any(is.na(args$order$orderComboLegs)))
      warning("perLegPrice: not all filled")

    # Conversions
    args$order$totalQuantity <- as.numeric(args$order$totalQuantity)
    args$order$filledQuantity <- as.numeric(args$order$filledQuantity)

    for(n in c("routeMarketableToBbo", "usePriceMgmtAlgo", "seekPriceImprovement")) {

      val <- args$order[[n]]

      if(val %in% c(0L, 1L))
        args$order[[n]] <- as.logical(val)
      else if(!is.na(val))
        warning("unexpected ", n, ": ", val)
    }

    validatepb("completedOrder", args)
  },

  # COMPLETED_ORDERS_END
  "302"= function(msg, ver) validatepb("completedOrdersEnd", list()),

  # REPLACE_FA_END
  "303"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("replaceFAEnd", splat(pb))
  },

  # WSH_META_DATA
  "304"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("wshMetaData", splat(pb))
  },

  # WSH_EVENT_DATA
  "305"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("wshEventData", splat(pb))
  },

  # HISTORICAL_SCHEDULE
  "306"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalSchedule, msg)

    validatepb("historicalSchedule", splat(pb))
  },

  # USER_INFO
  "307"= function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.StringData, msg)

    validatepb("userInfo", list(reqId=pb$reqId, whiteBrandingId=pb$data))
  },

  # HISTORICAL_DATA_END
  "308" = function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.HistoricalDataEnd, msg)

    validatepb("historicalDataEnd", splat(pb))
  },

  # CURRENT_TIME_IN_MILLIS
  "309" = function(msg, ver) {

    pb <- RProtoBuf::read(IBProto.SingleInt64, msg)

    validatepb("currentTimeInMillis", list(timeInMillis=pb$value))
  }

))
