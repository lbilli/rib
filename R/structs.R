#
# IB structs
#
CommissionReport <- list(execId=              "",
                         commission=          0,
                         currency=            "",
                         realizedPNL=         0,
                         yield=               0,
                         yieldRedemptionDate= 0L)     # yyyymmdd format

Execution <- list(execId=        "",
                  time=          "",
                  acctNumber=    "",
                  exchange=      "",
                  side=          "",
                  shares=        0,
                  price=         0,
                  permId=        0L,
                  clientId=      0L,
                  orderId=       0L,
                  liquidation=   0L,
                  cumQty=        0,
                  avgPrice=      0,
                  orderRef=      "",
                  evRule=        "",
                  evMultiplier=  0,
                  modelCode=     "",
                  lastLiquidity= 0L,
                  pendingPriceRevision= FALSE,
                  submitter=     "")

ExecutionFilter <- list(clientId= 0L,
                        acctCode= "",
                        time=     "",
                        symbol=   "",
                        secType=  "",
                        exchange= "",
                        side=     "")

ComboLeg  <- list(conId=              0L,
                  ratio=              0L,
                  action=             "",
                  exchange=           "",
                  openClose=          0L,
                  shortSaleSlot=      0L,
                  designatedLocation= "",
                  exemptCode=         -1L)

DeltaNeutralContract <- list(conId= 0L,
                             delta= 0,
                             price= 0)

Contract  <- list(conId=                        0L,
                  symbol=                       "",
                  secType=                      "",
                  lastTradeDateOrContractMonth= "", # xxxx [xxxx [xxxx]]
                                                    # - lastTradedDateOrContractMonth / bond maturity
                                                    # - ContractDetails$lastTradeTime
                                                    # - ContractDetails$timeZoneId
                  strike=                       0,
                  right=                        "",
                  multiplier=                   "",
                  exchange=                     "",
                  primaryExchange=              "",
                  currency=                     "",
                  localSymbol=                  "",
                  tradingClass=                 "",
                  includeExpired=               FALSE,
                  secIdType=                    "",
                  secId=                        "",
                  description=                  "",
                  issuerId=                     "",
                  lastTradeDate=                "",
                  comboLegsDescrip=             "",
                  comboLegs=                    list(),
                  deltaNeutralContract=         NA)

ContractDetails <- list(contract=           Contract,
                        marketName=         "",
                        minTick=            0,
                        orderTypes=         "",
                        validExchanges=     "",
                        priceMagnifier=     0L,
                        underConId=         0L,
                        longName=           "",
                        contractMonth=      "",
                        industry=           "",
                        category=           "",
                        subcategory=        "",
                        timeZoneId=         "",
                        tradingHours=       "",
                        liquidHours=        "",
                        evRule=             "",
                        evMultiplier=       NA_real_,
                        aggGroup=           NA_integer_,
                        underSymbol=        "",
                        underSecType=       "",
                        marketRuleIds=      "",
                        realExpirationDate= "",
                        lastTradeTime=      "",
                        stockType=          "",
                        minSize=            NA_real_,
                        sizeIncrement=      NA_real_,
                        suggestedSizeIncrement= NA_real_,
                        secIdList=          character(),
                        cusip=              "",
                        ratings=            "",
                        descAppend=         "",
                        bondType=           "",
                        couponType=         "",
                        callable=           FALSE,
                        putable=            FALSE,
                        coupon=             NA_real_,
                        convertible=        FALSE,
                        maturity=           "",
                        issueDate=          "",
                        nextOptionDate=     "",
                        nextOptionType=     "",
                        nextOptionPartial=  FALSE,
                        notes=              "",
                        fundName=                        "",
                        fundFamily=                      "",
                        fundType=                        "",
                        fundFrontLoad=                   "",
                        fundBackLoad=                    "",
                        fundBackLoadTimeInterval=        "",
                        fundManagementFee=               "",
                        fundClosed=                      FALSE,
                        fundClosedForNewInvestors=       FALSE,
                        fundClosedForNewMoney=           FALSE,
                        fundNotifyAmount=                "",
                        fundMinimumInitialPurchase=      "",
                        fundSubsequentMinimumPurchase=   "",
                        fundBlueSkyStates=               "",
                        fundBlueSkyTerritories=          "",
                        fundDistributionPolicyIndicator= "",
                        fundAssetType=                   "",
                        ineligibilityReasonList=         list())

ContractDescription <- list(contract=           Contract,
                            derivativeSecTypes= character())

OrderState  <- list(status=               "",
                    initMarginBefore=     "",
                    maintMarginBefore=    "",
                    equityWithLoanBefore= "",
                    initMarginChange=     "",
                    maintMarginChange=    "",
                    equityWithLoanChange= "",
                    initMarginAfter=      "",
                    maintMarginAfter=     "",
                    equityWithLoanAfter=  "",
                    commission=           NA_real_,
                    minCommission=        NA_real_,
                    maxCommission=        NA_real_,
                    commissionCurrency=   "",
                    marginCurrency=       "",
                    initMarginBeforeOutsideRTH=     NA_real_,
                    maintMarginBeforeOutsideRTH=    NA_real_,
                    equityWithLoanBeforeOutsideRTH= NA_real_,
                    initMarginChangeOutsideRTH=     NA_real_,
                    maintMarginChangeOutsideRTH=    NA_real_,
                    equityWithLoanChangeOutsideRTH= NA_real_,
                    initMarginAfterOutsideRTH=      NA_real_,
                    maintMarginAfterOutsideRTH=     NA_real_,
                    equityWithLoanAfterOutsideRTH=  NA_real_,
                    suggestedSize=        NA_real_,
                    rejectReason=         "",
                    orderAllocations=     list(),
                    warningText=          "",
                    completedTime=        "",
                    completedStatus=      "")

SoftDollarTier <- list(name=        "",
                       val=         "",
                       displayName= "")

Order <- list(orderId=                        0L,
              clientId=                       0L,
              permId=                         0L,
              action=                         "",
              totalQuantity=                  0,
              orderType=                      "",
              lmtPrice=                       NA_real_,
              auxPrice=                       NA_real_,
              tif=                            "",
              activeStartTime=                "",
              activeStopTime=                 "",
              ocaGroup=                       "",
              ocaType=                        0L,
              orderRef=                       "",
              transmit=                       TRUE,
              parentId=                       0L,
              blockOrder=                     FALSE,
              sweepToFill=                    FALSE,
              displaySize=                    NA_integer_,
              triggerMethod=                  0L,
              outsideRth=                     FALSE,
              hidden=                         FALSE,
              goodAfterTime=                  "",
              goodTillDate=                   "",
              rule80A=                        "",
              allOrNone=                      FALSE,
              minQty=                         NA_integer_,
              percentOffset=                  NA_real_,
              overridePercentageConstraints=  FALSE,
              trailStopPrice=                 NA_real_,
              trailingPercent=                NA_real_,
              faGroup=                        "",
              faMethod=                       "",
              faPercentage=                   "",
              openClose=                      "",
              origin=                         "CUSTOMER",
              shortSaleSlot=                  0L,
              designatedLocation=             "",
              exemptCode=                     -1L,
              discretionaryAmt=               0,
              optOutSmartRouting=             FALSE,
              auctionStrategy=                "UNSET",
              startingPrice=                  NA_real_,
              stockRefPrice=                  NA_real_,
              delta=                          NA_real_,
              stockRangeLower=                NA_real_,
              stockRangeUpper=                NA_real_,
              randomizeSize=                  FALSE,
              randomizePrice=                 FALSE,
              volatility=                     NA_real_,
              volatilityType=                 NA_integer_,
              deltaNeutralOrderType=          "",
              deltaNeutralAuxPrice=           NA_real_,
              deltaNeutralConId=              0L,
              deltaNeutralSettlingFirm=       "",
              deltaNeutralClearingAccount=    "",
              deltaNeutralClearingIntent=     "",
              deltaNeutralOpenClose=          "",
              deltaNeutralShortSale=          FALSE,
              deltaNeutralShortSaleSlot=      0L,
              deltaNeutralDesignatedLocation= "",
              continuousUpdate=               FALSE,
              referencePriceType=             NA_integer_,
              basisPoints=                    NA_real_,
              basisPointsType=                NA_integer_,
              scaleInitLevelSize=             NA_integer_,
              scaleSubsLevelSize=             NA_integer_,
              scalePriceIncrement=            NA_real_,
              scalePriceAdjustValue=          NA_real_,
              scalePriceAdjustInterval=       NA_integer_,
              scaleProfitOffset=              NA_real_,
              scaleAutoReset=                 FALSE,
              scaleInitPosition=              NA_integer_,
              scaleInitFillQty=               NA_integer_,
              scaleRandomPercent=             FALSE,
              scaleTable=                     "",
              hedgeType=                      "",
              hedgeParam=                     "",
              account=                        "",
              settlingFirm=                   "",
              clearingAccount=                "",
              clearingIntent=                 "",
              algoStrategy=                   "",
              algoParams=                     character(),
              smartComboRoutingParams=        character(),
              algoId=                         "",
              whatIf=                         FALSE,
              notHeld=                        FALSE,
              solicited=                      FALSE,
              modelCode=                      "",
              orderComboLegs=                 numeric(),
              orderMiscOptions=               character(),
              referenceContractId=            0L,
              peggedChangeAmount=             0,
              isPeggedChangeAmountDecrease=   FALSE,
              referenceChangeAmount=          0,
              referenceExchangeId=            "",
              adjustedOrderType=              "",
              triggerPrice=                   NA_real_,
              adjustedStopPrice=              NA_real_,
              adjustedStopLimitPrice=         NA_real_,
              adjustedTrailingAmount=         NA_real_,
              adjustableTrailingUnit=         0L,
              lmtPriceOffset=                 NA_real_,
              conditions=                     list(),
              conditionsCancelOrder=          FALSE,
              conditionsIgnoreRth=            FALSE,
              extOperator=                    "",
              softDollarTier=                 SoftDollarTier,
              cashQty=                        NA_real_,
              mifid2DecisionMaker=            "",
              mifid2DecisionAlgo=             "",
              mifid2ExecutionTrader=          "",
              mifid2ExecutionAlgo=            "",
              dontUseAutoPriceForHedge=       FALSE,
              isOmsContainer=                 FALSE,
              discretionaryUpToLimitPrice=    FALSE,
              autoCancelDate=                 "",
              filledQuantity=                 NA_real_,
              refFuturesConId=                NA_integer_,
              autoCancelParent=               FALSE,
              shareholder=                    "",
              imbalanceOnly=                  FALSE,
              routeMarketableToBbo=           FALSE,
              parentPermId=                   NA_integer_,
              usePriceMgmtAlgo=               NA,
              duration=                       NA_integer_,
              postToAts=                      NA_integer_,
              advancedErrorOverride=          "",
              manualOrderTime=                "",
              minTradeQty=                    NA_integer_,
              minCompeteSize=                 NA_integer_,
              competeAgainstBestOffset=       NA_real_,
              midOffsetAtWhole=               NA_real_,
              midOffsetAtHalf=                NA_real_,
              customerAccount=                "",
              professionalCustomer=           FALSE,
              bondAccruedInterest=            "",
              includeOvernight=               FALSE,
              manualOrderIndicator=           NA_integer_,
              submitter=                      "")

OrderCancel <- list(manualOrderCancelTime= "",
                    extOperator=           "",
                    manualOrderIndicator=  NA_integer_)

ScannerSubscription <- list(numberOfRows=             -1L,
                            instrument=               "",
                            locationCode=             "",
                            scanCode=                 "",
                            abovePrice=               NA_real_,
                            belowPrice=               NA_real_,
                            aboveVolume=              NA_integer_,
                            marketCapAbove=           NA_real_,
                            marketCapBelow=           NA_real_,
                            moodyRatingAbove=         "",
                            moodyRatingBelow=         "",
                            spRatingAbove=            "",
                            spRatingBelow=            "",
                            maturityDateAbove=        "",
                            maturityDateBelow=        "",
                            couponRateAbove=          NA_real_,
                            couponRateBelow=          NA_real_,
                            excludeConvertible=       FALSE,
                            averageOptionVolumeAbove= NA_integer_,
                            scannerSettingPairs=      "",
                            stockTypeFilter=          "")

WshEventData <- list(conId=           NA_integer_,
                     filter=          "",
                     fillWatchlist=   FALSE,
                     fillPortfolio=   FALSE,
                     fillCompetitors= FALSE,
                     startDate=       "",
                     endDate=         "",
                     totalLimit=      NA_integer_)
