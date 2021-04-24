#
# Some constants
#
API_SIGN <- writeBin("API", raw()) # "API\0" (the null termination is added automatically by writeBin()

HEADER_LEN  <- 4L
MAX_MSG_LEN <- 0xFFFFFFL  # 16Mb - 1b

# Server Versions
MIN_SERVER_VER_PRICE_BASED_VOLATILITY  <- 156L
MIN_SERVER_VER_REPLACE_FA_END          <- 157L
MIN_SERVER_VER_DURATION                <- 158L

MIN_CLIENT_VER <- 155L
MAX_CLIENT_VER <- MIN_SERVER_VER_DURATION
