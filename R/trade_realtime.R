.trade_realtime <- function(ib_connect, Contract,
          whatToShow="TRADES",
          barSize="5",useRTH=TRUE,
          tickerId = "1") {
  if(!is.twsConnection(ib_connect))
    stop("tws connection object required")
  if(is.twsContract(Contract))
    Contract <- list(Contract)

  for(n in 1:length(Contract)) {
    if(!is.twsContract(Contract[[n]]))
       stop("twsContract required")
  }
  sock_et <- ib_connect[[1]]
  if(!isOpen(sock_et))
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
    writeBin(request, sock_et)
#    ticker_id <- as.character(as.numeric(tickerId))
#    ticker_ids[n] <- ticker_id
  }
  ticker_id
}  # end .trade_realtime

#' @export
trade_realtime <- function(ib_connect, Contract,
                             whatToShow="TRADES",
                             barSize="5",useRTH=TRUE,
                             playback = 1,
                             tickerId = "1",
                             file = "",
                             verbose=TRUE,
                             eventWrapper=eWrapper(),
                             CALLBACK=twsCALLBACK, ...) {
    if(!is.twsPlayback(ib_connect)) {
      tickerId <- .trade_realtime(ib_connect, Contract, whatToShow, barSize, useRTH, tickerId)
    }

    if(is.twsContract(Contract))
      Contract <- list(Contract)

    sock_et <- ib_connect[[1]]
    cancel_trade_realtime <- function(sock_et, tickerId) {
      if(inherits(sock_et, 'sockconn')) {
        for(i in 1:length(tickerId)) {
          writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS, sock_et)
          writeBin('1', sock_et)
          writeBin(tickerId[i], sock_et)
        }
      } else {
        seek(sock_et, 0)
      }
    }

    if(is.null(CALLBACK))
      CALLBACK <- twsDEBUG # function to simply return raw data

    if(!missing(CALLBACK) && is.na(list(CALLBACK))) {
      if(is.twsPlayback(ib_connect)) {
        seek(ib_connect[[1]], 0)
        stop("CALLBACK=NA is not available for playback")
      }
      return(tickerId)
    }
    on.exit(cancel_trade_realtime(sock_et, tickerId))

    symbol.or.local <- function(x) {
      # used to find best name for id in output
      symbol <- x$symbol
      local  <- x$local
      if(local=="") {
        return(symbol)
      } else return(local)
    }
    eventWrapper$as_sign("symbols", sapply(Contract, symbol.or.local))
    # data is list of vectors TimeStamp, Open, High, Low, Close, Volume, VWAP, Count
    # eventWrapper$assign.Data("data", rep(list(rep(NA, 8)), length(Contract)))

    timeStamp <- NULL
    if(!is.list(file))
      file <- list(file)
    if(length(file) != length(Contract))
      file <- rep(file, length(Contract))
    CALLBACK(ib_connect, eWrapper=eventWrapper, timestamp=timeStamp, file=file,
             playback=playback, ...)
}  # end trade_realtime

`cancel_trade_realtime` <- function(ib_connect,tickerId) {
  if(!is.twsConnection(ib_connect))
    stop("twsConnection object required")

  sock_et <- ib_connect[[1]]

  for(i in 1:length(tickerId)) {
    writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS, sock_et)
    writeBin('1', sock_et)
    writeBin(tickerId[i], sock_et)
  }
}  # end cancel_trade_realtime
