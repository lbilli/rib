#
# Some constants
#
API_SIGN <- writeBin("API", raw()) # "API\0" (the null termination is added automatically by writeBin()

HEADER_LEN  <- 4L
MAX_MSG_LEN <- 0xFFFFFFL  # 16Mb - 1b

RAWID_LEN <- 4L
PROTOBUF_MSG_ID <- 200L

# Server Versions
MIN_SERVER_VER_USE_PRECISION_FROM_SEC_DEF       <- 224L
MIN_SERVER_VER_ODD_LOT_BID_ASK_QUOTES           <- 225L
COND_ORDER_WITH_OVERNIGHT_PARAM                 <- 226L


MIN_CLIENT_VER <- 223L
MAX_CLIENT_VER <- COND_ORDER_WITH_OVERNIGHT_PARAM
