#
# Some constants
#
API_SIGN <- writeBin("API", raw()) # "API\0" (the null termination is added automatically by writeBin()

HEADER_LEN  <- 4L
MAX_MSG_LEN <- 0xFFFFFFL  # 16Mb - 1b

# Server Versions
MIN_SERVER_VER_FA_PROFILE_DESUPPORT               <- 177L
MIN_SERVER_VER_PENDING_PRICE_REVISION             <- 178L
MIN_SERVER_VER_FUND_DATA_FIELDS                   <- 179L
MIN_SERVER_VER_MANUAL_ORDER_TIME_EXERCISE_OPTIONS <- 180L
MIN_SERVER_VER_OPEN_ORDER_AD_STRATEGY             <- 181L
MIN_SERVER_VER_LAST_TRADE_DATE                    <- 182L
MIN_SERVER_VER_CUSTOMER_ACCOUNT                   <- 183L
MIN_SERVER_VER_PROFESSIONAL_CUSTOMER              <- 184L
MIN_SERVER_VER_BOND_ACCRUED_INTEREST              <- 185L
MIN_SERVER_VER_INELIGIBILITY_REASONS              <- 186L
MIN_SERVER_VER_RFQ_FIELDS                         <- 187L
MIN_SERVER_VER_BOND_TRADING_HOURS                 <- 188L
MIN_SERVER_VER_INCLUDE_OVERNIGHT                  <- 189L
MIN_SERVER_VER_UNDO_RFQ_FIELDS                    <- 190L
MIN_SERVER_VER_PERM_ID_AS_LONG                    <- 191L
MIN_SERVER_VER_CME_TAGGING_FIELDS                 <- 192L
MIN_SERVER_VER_CME_TAGGING_FIELDS_IN_OPEN_ORDER   <- 193L


MIN_CLIENT_VER <- 176L
MAX_CLIENT_VER <- MIN_SERVER_VER_CME_TAGGING_FIELDS_IN_OPEN_ORDER
