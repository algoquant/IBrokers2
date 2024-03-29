.reqRealTimeBars <- function(conn, Contract,
          whatToShow="TRADES",
          barSize="5",useRTH=TRUE,
          tickerId = "1") {
  if(!is.twsConnection(conn))
    stop("tws connection object required")
  if(is.twsContract(Contract))
    Contract <- list(Contract)

  for(n in 1:length(Contract)) {
    if(!is.twsContract(Contract[[n]]))
       stop("twsContract required")
  }
  con <- conn[[1]]
  if(!isOpen(con))
    stop("connection to TWS has been closed")


  VERSION <- "1"
  if(length(tickerId) != length(Contract))
    tickerId <- seq(as.numeric(tickerId), length.out=length(Contract))

  ticker_id <- as.character(tickerId)

  for(n in 1:length(Contract)) {
    request <- c(.twsOutgoingMSG$REQ_REAL_TIME_BARS, VERSION, ticker_id[n],
        Contract[[n]]$symbol,
        Contract[[n]]$sectype,
        Contract[[n]]$expiry,
        Contract[[n]]$strike,
        Contract[[n]]$right,
        Contract[[n]]$multiplier,
        Contract[[n]]$exch,
        Contract[[n]]$primary,
        Contract[[n]]$currency,
        Contract[[n]]$local,barSize,whatToShow,
        as.character(as.numeric(useRTH)))
    writeBin(request, con)
#    ticker_id <- as.character(as.numeric(tickerId))
#    ticker_ids[n] <- ticker_id
  }
  ticker_id
}

#' @export
reqRealTimeBars <- function(conn, Contract,
                             whatToShow="TRADES",
                             barSize="5",useRTH=TRUE,
                             playback = 1,
                             tickerId = "1",
                             file = "",
                             verbose=TRUE,
                             eventWrapper=eWrapper(),
                             CALLBACK=twsCALLBACK, ...) {
    if(!is.twsPlayback(conn)) {
      tickerId <- .reqRealTimeBars(conn, Contract, whatToShow, barSize, useRTH, tickerId)
    }

    if(is.twsContract(Contract))
      Contract <- list(Contract)

    con <- conn[[1]]
    cancelRealTimeBars <- function(con,tickerId) {
      if(inherits(con,'sockconn')) {
        for(i in 1:length(tickerId)) {
          writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS,con)
          writeBin('1',con)
          writeBin(tickerId[i],con)
        }
      } else {
        seek(con, 0)
      }
    }

    if(is.null(CALLBACK))
      CALLBACK <- twsDEBUG # function to simply return raw data

    if(!missing(CALLBACK) && is.na(list(CALLBACK))) {
      if(is.twsPlayback(conn)) {
        seek(conn[[1]], 0)
        stop("CALLBACK=NA is not available for playback")
      }
      return(tickerId)
    }
    on.exit(cancelRealTimeBars(con, tickerId))

    symbol.or.local <- function(x) {
      # used to find best name for id in output
      symbol <- x$symbol
      local  <- x$local
      if(local=="") {
        return(symbol)
      } else return(local)
    }
    eventWrapper$assign.Data("symbols", sapply(Contract, symbol.or.local))
    # data is list of vectors TimeStamp, Open, High, Low, Close, Volume, VWAP, Count
    # eventWrapper$assign.Data("data", rep(list(rep(NA, 8)), length(Contract)))

    timeStamp <- NULL
    if(!is.list(file))
      file <- list(file)
    if(length(file) != length(Contract))
      file <- rep(file, length(Contract))
    CALLBACK(conn, eWrapper=eventWrapper, timestamp=timeStamp, file=file,
             playback=playback, ...)
}  # end reqRealTimeBars

`cancelRealTimeBars` <- function(conn,tickerId) {
  if(!is.twsConnection(conn))
    stop("twsConnection object required")

  con <- conn[[1]]

  for(i in 1:length(tickerId)) {
    writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS,con)
    writeBin('1',con)
    writeBin(tickerId[i],con)
  }
}
