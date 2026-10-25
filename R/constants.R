#
# Some constants
#
API_SIGN <- writeBin("API", raw()) # "API\0" (the null termination is added automatically by writeBin()

HEADER_LEN  <- 4L
MAX_MSG_LEN <- 0xFFFFFFL  # 16Mb - 1b

RAWID_LEN <- 4L
PROTOBUF_MSG_ID <- 200L

# Server Versions
COND_ORDER_WITH_OVERNIGHT_PARAM                 <- 226L


MIN_CLIENT_VER <- 226L
MAX_CLIENT_VER <- COND_ORDER_WITH_OVERNIGHT_PARAM
