#
# Some constants
#
API_SIGN <- writeBin("API", raw()) # "API\0" (the null termination is added automatically by writeBin()

HEADER_LEN  <- 4L
MAX_MSG_LEN <- 0xFFFFFFL  # 16Mb - 1b

RAWID_LEN <- 4L
PROTOBUF_MSG_ID <- 200L

# Server Versions
COMBO_IMPLIED_BID_ASK            <- 227L


MIN_CLIENT_VER <- 226L
MAX_CLIENT_VER <- COMBO_IMPLIED_BID_ASK
