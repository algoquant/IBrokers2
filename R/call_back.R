# The function IBrokers2::call_back() performs a callback loop,
# similar to IBrokers::twsCALLBACK().
# It is a clone of IBrokers::twsCALLBACK().
# It is designed to be called by IBrokers2::trade_realtime().
# The argument back_test determines if the callback loop
# receives live data (back_test=FALSE), or it plays back
# historical data (back_test=TRUE).

#' @export
call_back <- function(ib_connect, e_wrapper, timestamp, file, back_test=FALSE, sym_bol="ES", ...) {

  sock_et <- ib_connect[[1]]

  if (missing(e_wrapper))
    e_wrapper <- IBrokers2::eWrapper()

  if (back_test) {
    # Code for off-line backtest
    # Load the data from file
    data_dir <- "C:/Develop/data/ib_data/"
    load(paste0(data_dir, sym_bol, "_ohlc.RData"))

    n_rows <- NROW(oh_lc)
    e_wrapper$da_ta$n_rows <- n_rows
    n_cols <- e_wrapper$da_ta$n_cols
    n_contracts <- e_wrapper$da_ta$n_contracts
    name_s <- e_wrapper$da_ta$name_s

    ohlc_data <- coredata(oh_lc)
    ohlc_data <- cbind(rep(1, n_rows), rep(1, n_rows), xts::.index(oh_lc), ohlc_data)

    # Create data buffer bar_data, a list of matrices in the e_wrapper environment
    # The old buffer bar_data needs to be resized
    e_wrapper$da_ta$bar_data <- rep(list(matrix(rep(NA_real_, n_rows*n_cols), ncol=n_cols)), n_contracts)
    names(e_wrapper$da_ta$bar_data) <- name_s
    for (it in 1:NROW(name_s)) {
      col_n <- paste(name_s[it], e_wrapper$da_ta$col_names, sep=".")
      colnames(e_wrapper$da_ta$bar_data[[it]]) <- col_n
    }  # end for

    # perform callback backtest loop
    for (it in 1:n_rows) {
      new_bar <- ohlc_data[it, ]
      e_wrapper$realtimeBars(curMsg=1, msg=new_bar, timestamp=new_bar[2], file=file, ib_connect=ib_connect, ...)
    }  # end for

  } else {
    # Code for live trading
    tryCatch(
      # Callback loop
      while (isConnected(ib_connect)) {
        if (!socketSelect(list(sock_et), FALSE, 0.25))
          next
        curMsg <- readBin(sock_et, "character", 1L)
        # Process the message
        if (!is.null(timestamp)) {
          processMsg(curMsg, sock_et, e_wrapper, format(Sys.time(), timestamp), file, ib_connect, ...)
        } else {
          processMsg(curMsg, sock_et, e_wrapper, timestamp, file, ib_connect, ...)
        }  # end if
      },  # end while
      # error handler
      error=function(e) { close(ib_connect); stop("IB connection error. Connection closed", call.=FALSE) }
    )  # end tryCatch
  }  # end if
}  # end call_back
