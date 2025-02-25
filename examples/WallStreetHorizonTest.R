#' Test Wall Street Horizon Data Retrieval via TWS API
#'
#' This function demonstrates the retrieval of Wall Street Horizon (WSH) data from
#' Interactive Brokers' Trader Workstation (TWS) API. It sends two types of requests:
#'
#' \itemize{
#'   \item A meta data request (message ID "100") using a custom \code{MetaCollector} class.
#'   \item An event data request (message ID "102") with a JSON filter using a custom
#'         \code{WSHCollector} class.
#' }
#'
#' The WSH event data request uses a JSON filter similar to the examples provided in
#' the official TWS API documentation, see:
#' \url{https://interactivebrokers.github.io/tws-api/wshe_filters.html}
#'
#' The function establishes a connection to TWS, issues the requests, and waits for the
#' corresponding callbacks to complete the data retrieval. The raw JSON data returned from TWS
#' is stored in the \code{rawData} field of the \code{WSHCollector} object, which is then returned.
#'
#' @return An object of class \code{WSHCollector} (an R6 object) with the following fields:
#' \describe{
#'   \item{rawData}{A character string containing the raw JSON data returned from TWS.}
#'   \item{done}{A logical flag indicating whether the WSH event data retrieval has completed.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Retrieve Wall Street Horizon data from TWS.
#'   collector <- testWSH()
#'   if (collector$done) {
#'     cat("Retrieved data:", collector$rawData)
#'   } else {
#'     cat("Data retrieval incomplete.")
#'   }
#' }
#'
#' @seealso
#' \itemize{
#'   \item \url{https://interactivebrokers.github.io/tws-api/wshe_filters.html} for details on WSH filters.
#'   \item \code{\link{IBClient}} for managing the TWS connection.
#' }
#'
#' @export
testWSH <- function() {
  on.exit(ic$disconnect())        # Guarantee a disconnect when finished
  try(ic$disconnect(), silent=TRUE)

  #############################
  # 1) CONNECT
  #############################
  ic <- IBClient$new()
  client_id <- 1
  ic$connect("127.0.0.1", 7497, clientId = client_id)

  #############################
  # 2) REQ WSH META DATA
  #############################
  MetaCollector <- R6::R6Class(
    "MetaCollector",
    inherit = IBWrap,
    cloneable = FALSE,
    public = list(
      done = FALSE,
      wshMetaData = function(reqId, data) {
        cat("[MetaCollector] wshMetaData received.\n")
      },
      wshMetaDataEnd = function(reqId) {
        cat("[MetaCollector] wshMetaDataEnd.\n")
        self$done <- TRUE
      },
      error = function(id, errorTime, errorCode, errorString, advancedOrderRejectJson) {
        cat("[MetaCollector] error:", errorString, "\n")
      }
    )
  )$new()

  reqId_meta <- 1
  ic$reqWshMetaData(reqId_meta)

  start_time <- Sys.time()
  while (!MetaCollector$done && (Sys.time() - start_time < 10)) {
    # No debug=TRUE needed now
    ic$checkMsg(MetaCollector, timeout=0.2)
  }

  ic$cancelWshMetaData(reqId_meta)

  #############################
  # 3) REQ WSH EVENT DATA
  #############################
  WSHCollector <- R6::R6Class(
    "WSHCollector",
    inherit = IBWrap,
    cloneable = FALSE,
    public = list(
      rawData = "",
      done = FALSE,
      wshEventData = function(reqId, dataJson) {
        cat("[WSHCollector] wshEventData received chunk:\n", reqId, dataJson, "\n")
        if (nzchar(self$rawData)) {
          self$rawData <- paste0(self$rawData, ",", dataJson)
        } else {
          self$rawData <- dataJson
        }
        self$done <- TRUE
      },
      error = function(id, time, code, msg, advJson) {
        cat("[WSHCollector] error:", msg, "\n")
      }
    )
  )$new()

  jsonFilter <- '{
      "country": "All",
      "watchlist": ["8314"],
      "limit_region": 10,
      "limit": 10,
      "wshe_ed": "true",
      "wshe_bod": "true"
  }'

  wshData <- list(
    conId         = 0,     # 0 means we’re supplying a JSON filter
    filter        = jsonFilter,
    fillWatchlist = FALSE,
    fillPortfolio = FALSE,
    fillCompetitors = FALSE,
    startDate     = "",
    endDate       = "",
    totalLimit    = "2147483647"
  )

  reqId_wsh <- 2
  ic$reqWshEventData(reqId_wsh, wshData)

  start_time <- Sys.time()
  while (!WSHCollector$done && (Sys.time() - start_time < 15)) {
    # No debug=TRUE → decode still calls WSHCollector#wshData
    ic$checkMsg(WSHCollector, timeout=0.2)
  }

  if (!WSHCollector$done) {
    cat("[INFO] WSHCollector$done: ", WSHCollector$done, "\n")
    ic$cancelWshEventData(reqId_wsh)
    # If TWS never sends a “wshEventDataEnd,” you may want to set
    #   WSHCollector$done <- TRUE here.
  }

  cat("[INFO] TWS returned data:\n", WSHCollector$rawData, "\n")

  return(WSHCollector)
}

#' Parse WSH Raw Data into a Friendly Data Table
#'
#' This function parses a raw JSON string (as returned by the TWS API for WSH event data)
#' and returns either a single \code{data.table} (with one row per event) or a named list
#' of \code{data.table}s grouped by event type. To avoid warnings from \code{data.table} about
#' recycling vector lengths, any field that is a vector of length > 1 is collapsed into a single string.
#'
#' @param rawData A JSON string containing the raw WSH event data.
#' @param groupByEventType Logical. If \code{TRUE}, the output is a list of \code{data.table}s
#'   grouped by event type; if \code{FALSE} (the default), all events are combined into a single \code{data.table}.
#'
#' @return A single \code{data.table} (if \code{groupByEventType = FALSE}) or a named list of \code{data.table}s (if \code{groupByEventType = TRUE}).
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.table as.data.table rbindlist
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#'   # Example with two events:
#'   rawData <- '[{"index_date_type":"DATE",
#'                "event_type":"wshe_ic",
#'                "data":{"end_date":"20200101"
#'                ,"purl":"http://example.com","time_zone":"EST"},
#'                "filterSource":"watchlist",
#'                "conids":["1","2"],
#'                "index_date":"20200101",
#'                "source":"WSHE",
#'                "event_key":"E1234",
#'                "tooltips":{},
#'                "status":""},
#'               {"index_date_type":"DATE",
#'                "event_type":"wshe_ed",
#'                "data":{"end_date":"20200102"
#'                ,"purl":"http://example.org","time_zone":"EST"},
#'                "filterSource":"watchlist",
#'                "conids":["3","4"],
#'                "index_date":"20200102",
#'                "source":"WSHE",
#'                "event_key":"E5678",
#'                "tooltips":{},
#'                "status":""}]'
#'
#'   # Return a single data.table:
#'   dt_all <- parseWSHData(rawData, groupByEventType = FALSE)
#'   print(dt_all)
#'
#'   # Or return a list of data.tables by event type:
#'   dt_list <- parseWSHData(rawData, groupByEventType = TRUE)
#'   print(dt_list)
#' }
#'
#' @export
parseWSHData <- function(rawData, groupByEventType = FALSE) {
  stopifnot(is.character(rawData))
  # Parse the JSON string with flatten=TRUE so that nested objects become columns.
  parsed <- jsonlite::fromJSON(rawData, flatten = TRUE)
  dt <- data.table::as.data.table(parsed)

  # For any column that is a list, if each element is a character vector,
  # collapse it into a comma-separated string.
  fixListCols <- function(x) {
    if (!is.list(x)) return(x)
    # If every element is either NULL or a character vector, collapse each.
    sapply(x, function(item) {
      if (is.null(item)) return("")
      # If item is not already a character vector, coerce.
      item <- as.character(item)
      paste(item, collapse = ",")
    })
  }
  for (col in names(dt)) {
    if (is.list(dt[[col]])) {
      dt[[col]] <- fixListCols(dt[[col]])
    }
  }

  # Optionally, group by event_type
  if (groupByEventType) {
    dt_list <- split(dt, by = "event_type", keep.by = TRUE)
    return(dt_list)
  }

  return(dt)
}






#' Recursively Flatten a Nested List
#'
#' This helper function recursively flattens a nested list. The names of nested
#' elements are combined using a dot separator.
#'
#' @param x A list (possibly nested).
#' @param parent_key A character string for the parent key (default is \code{NULL}).
#'
#' @importFrom stats setNames
#'
#' @return A single-level named list.
flattenList <- function(x, parent_key = NULL) {
  out <- list()
  if (!is.list(x)) {
    # Not a list: return it as is with the parent key
    return(setNames(list(x), parent_key))
  }
  # For each element in x, flatten it recursively if needed.
  for (name in names(x)) {
    new_key <- if (!is.null(parent_key)) paste0(parent_key, ".", name) else name
    # If the element is itself a non-empty list with names, recurse.
    if (is.list(x[[name]]) && !is.null(names(x[[name]]))) {
      out <- c(out, flattenList(x[[name]], new_key))
    } else {
      # Otherwise, assign the element directly.
      out[[new_key]] <- x[[name]]
    }
  }
  out
}


