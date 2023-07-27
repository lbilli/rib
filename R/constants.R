#
# Some constants
#
API_SIGN <- writeBin("API", raw()) # "API\0" (the null termination is added automatically by writeBin()

HEADER_LEN  <- 4L
MAX_MSG_LEN <- 0xFFFFFFL  # 16Mb - 1b

# Server Versions
MIN_SERVER_VER_FA_PROFILE_DESUPPORT        <- 177L
MIN_SERVER_VER_PENDING_PRICE_REVISION      <- 178L


MIN_CLIENT_VER <- 176L
MAX_CLIENT_VER <- MIN_SERVER_VER_PENDING_PRICE_REVISION
