# n_instr is the number of instruments in the data buffer
trade_wrapper <- function(name_s=NULL, buy_spread=0.25, sell_spread=0.25, file_connects) {
  # cat("Entering trade_wrapper", "\n")
  # Create eWrapper environment
  ew_env <- create_ewrapper(NULL)
  # ew_env <- new.env()
  # Create eWrapper accessor functions
  # ew_env$get_ <- function(x) get(x, ew_env)
  # ew_env$assign_ <- function(x, value) assign(x, value, ew_env)
  # ew_env$remove_ <- function(x) remove(x, ew_env)
  # Initialize state variables in eWrapper environment
  # ew_env$assign.Data("count_er", 0)
  ew_env$count_er <- 0
  # Define dimensions of data buffer for single instrument
  col_names <- c("Time", "Open", "High", "Low", "Close", "Volume", "WAP", "Count")
  # ew_env$assign.Data("col_names", col_names)
  ew_env$col_names <- col_names
  n_row <- 8*60*12; n_col <- NROW(col_names)
  ew_env$n_row <- n_row
  ew_env$n_col <- n_col
  # ew_env$n_row <- 8*60*12; ew_env$n_col <- NROW(ew_env$col_names)
  if (is.null(name_s))
    stop("name_s argument is missing")
  else {
    n_instr <- NROW(name_s)
    ew_env$name_s <- name_s
  }  # end if

  # Create data buffer bar_data, as a list of matrices in the eWrapper environment
  ew_env$bar_data <- rep(list(matrix(rep(NA_real_, n_row*n_col), ncol=n_col)), n_instr)
  names(ew_env$bar_data) <- name_s
  for (it in 1:NROW(name_s)) {
    col_n <- paste(name_s[it], col_names, sep=".")
    colnames(ew_env$bar_data[[it]]) <- col_n
    # Write headers to data files
    cat(col_n, "\n", file=file_connects[[it]], append=TRUE)
  }  # end for

  # Create data buffer bar_data, as a list of data frames in the eWrapper environment
  # ew_env$bar_data <- rep(list(as.data.frame(matrix(rep(NA_real_, n_row*n_col), ncol=n_col))), n_instr)
  # Create data buffer bar_data, as a list of xts series in the eWrapper environment
  # ew_env$bar_data <- rep(list(structure(.xts(matrix(rep(NA_real_, n_row*n_col), ncol=n_col), 1:n_row),
  #                                       .Dimnames=list(NULL, col_names))),
  #                        n_instr)

  # Initialize trading model parameters
  ew_env$model_params$buy_spread <- buy_spread
  ew_env$model_params$sell_spread <- sell_spread
  # Initialize state variables in eWrapper environment
  ew_env$n_instr <- n_instr
  ew_env$buy_id <- 0
  ew_env$sell_id <- 0

  ## Define trading model function inside the eWrapper environment
  # The function model_fun is called from inside realtimeBars()
  ew_env$model_fun <- function(new_bar) {
    # if (!IBrokers2::isConnected(ib_connect)) {ib_connect <- IBrokers2::twsConnect(port=7497) ; cat("reconnected")}
    # Cancel previous trade orders
    # cat("model_fun: ", ew_env$count_er, "\n")
    if (ew_env$count_er > 1) {
      IBrokers2::cancelOrder(ib_connect, ew_env$buy_id)
      IBrokers2::cancelOrder(ib_connect, ew_env$sell_id)
    }  # end if

    # Execute buy limit order
    buy_id <- IBrokers2::reqIds(ib_connect)
    buy_price <- (new_bar["Low"] - ew_env$model_params$buy_spread)
    buy_order <- IBrokers2::twsOrder(buy_id, orderType="LMT",
                                     lmtPrice=buy_price, action="BUY", totalQuantity=1)
    IBrokers2::placeOrder(ib_connect, con_tracts[[1]], buy_order)

    # Execute sell limit order
    sell_id <- IBrokers2::reqIds(ib_connect)
    sell_price <- (new_bar["High"] + ew_env$model_params$sell_spread)
    sell_order <- IBrokers2::twsOrder(sell_id, orderType="LMT",
                                      lmtPrice=sell_price, action="SELL", totalQuantity=1)
    IBrokers2::placeOrder(ib_connect, con_tracts[[1]], sell_order)

    ew_env$buy_id <<- buy_id
    ew_env$sell_id <<- sell_id

    cat("Buy order at: ", buy_price, "\tSell order at: ", sell_price, "\n")

    invisible(c(buy_order=buy_price, sell_order=sell_price))
  }  # end model_fun


  # realtimeBars() processes a new bar of data and runs the model_fun()
  # realtimeBars() is called by processMsg() in a callback loop inside twsCALLBACK()
  ew_env$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    # cat("realtimeBars: ", ew_env$count_er, "\n")
    # ew_env$assign.Data("count_er", ew_env$get.Data("count_er")+1)
    ew_env$count_er <<- ew_env$count_er + 1
    # Unwrap new bar of data in msg passed from processMsg()
    new_bar <- as.numeric(msg)
    # cat("realtimeBars col_names: ", col_names, "\n")
    # cat("realtimeBars n_col: ", n_col, "\n")
    col_index <- (3:(ew_env$n_col+2))
    names(new_bar)[col_index] <- ew_env$col_names
    instr_id <- new_bar[2]
    # cat("realtimeBars new_bar: ", new_bar, "\n")
    # cat("realtimeBars: ", ew_env$get.Data("count_er"), "\n")
    # Copy new bar of data into buffer
    # cat("realtimeBars new_bar: ", new_bar, "\n")
    ew_env$bar_data[[instr_id]][ew_env$count_er, ] <<- new_bar[col_index]
    # cat("realtimeBars bar_data: ", ew_env$bar_data[[instr_id]][ew_env$count_er, ], "\n")
    # if (ew_env$count_er > 1)
    #   cat("realtimeBars bar_data: ", ew_env$bar_data[[instr_id]][ew_env$count_er-1, ], "\n")
    # if (ew_env$count_er > 2)
    #   cat("realtimeBars bar_data: ", ew_env$bar_data[[instr_id]][ew_env$count_er-2, ], "\n")
    # Write to file
    # file_name <- file[[instr_id]]
    cat(paste(ew_env$bar_data[[instr_id]][ew_env$count_er, ], collapse=","), "\n", file=file[[instr_id]], append=TRUE)
    # Write to file and add instr_id
    # cat(paste(ew_env$name_s[[instr_id]], paste(ew_env$bar_data[[instr_id]][ew_env$count_er, ], collapse=","), sep=","), "\n",
    #     file=file[[instr_id]], append=TRUE)
    # Write to file every 10 counts
    # if ((ew_env$count_er %% 10) == 0) {
    #   for (instr_id in 1:(ew_env$n_instr)) {
    #     data.table::fwrite(ew_env$bar_data[[ew_env$instr_id]], file=file[[ew_env$instr_id]])
    #   }  # end for
    # }  # end if
    # Write to console
    # cat(c(ew_env$name_s[[instr_id]], paste(ew_env$bar_data[[instr_id]][ew_env$count_er, ], collapse=",")), "\n")
    cat(paste0("count_er=", ew_env$count_er), paste0(ew_env$col_names, "=", new_bar[col_index]), "\n")
    # cat("Number of rows of data for instrument ", instr_id, " is = ", NROW(ew_env$bar_data[[ew_env$instr_id]]), "\n")
    # cat(paste0("Open=", new_bar[4], "\tHigh=", new_bar[5], "\tLow=", new_bar[6], "\tClose=", new_bar[7], "\tVolume=", new_bar[8]), "\n")
    # Run the trading model
    if (ew_env$name_s[[instr_id]]=="es")
      ew_env$model_fun(new_bar)
    # Return values
    c(curMsg, msg)
  }  # end realtimeBars

  return(ew_env)
}  # end trade_wrapper




#' @details The function \code{create_ewrapper()} creates an eWrapper
#'   environment combined with handler functions.
#' @export
create_ewrapper <- function(debug=FALSE, errfile=stderr()) {
  .Data <- new.env()
  # Create accessor functions
  get.Data <- function(x) get(x, .Data)
  assign.Data <- function(x, value) assign(x, value, .Data)
  remove.Data <- function(x) remove(x, .Data)
  if (is.null(debug)) {
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      cat(msg, "\n", file=errfile)
    }
    tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
  }
  else if (!debug) {
    tickPrice <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      e_tick_price(NULL, msg, timestamp, file, symbols,
                   ...)
    }
    tickSize <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      e_tick_size(NULL, msg, timestamp, file, symbols,
                  ...)
    }
    tickOptionComputation <- function(curMsg, msg, timestamp,
                                      file, ...) {
      symbols <- get.Data("symbols")
      e_tick_option(NULL, msg, timestamp, file, symbols,
                    ...)
    }
    tickGeneric <- function(curMsg, msg, timestamp, file,
                            ...) {
      symbols <- get.Data("symbols")
      e_tick_generic(NULL, msg, timestamp, file, symbols,
                     ...)
    }
    tickString <- function(curMsg, msg, timestamp, file,
                           ...) {
      symbols <- get.Data("symbols")
      e_tick_string(NULL, msg, timestamp, file, symbols,
                    ...)
    }
    tickEFP <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      e_tick_EFP(NULL, msg, timestamp, file, symbols, ...)
    }
    orderStatus <- function(curMsg, msg, timestamp, file,
                            ...) {
      e_order_status(curMsg, msg)
      c(curMsg, msg)
    }
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      if (msg[3] == "1100")
        twsconn$connected <- FALSE
      if (msg[3] %in% c("1101", "1102"))
        twsconn$connected <- TRUE
      cat("TWS Message:", msg, "\n")
    }
    openOrder <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    openOrderEnd <- function(curMsg, msg, timestamp, file,
                             ...) {
      c(curMsg, msg)
    }
    updateAccountValue <- function(curMsg, msg, timestamp,
                                   file, ...) {
      c(curMsg, msg)
    }
    updatePortfolio <- function(curMsg, msg, timestamp, file,
                                ...) {
      e_portfolio_value(curMsg, msg)
      c(curMsg, msg)
    }
    updateAccountTime <- function(curMsg, msg, timestamp,
                                  file, ...) {
      c(curMsg, msg)
    }
    accountDownloadEnd <- function(curMsg, msg, timestamp,
                                   file, ...) {
      c(curMsg, msg)
    }
    nextValidId <- function(curMsg, msg, timestamp, file,
                            ...) {
      c(curMsg, msg)
    }
    contractDetails <- function(curMsg, msg, timestamp, file,
                                ...) {
      c(curMsg, msg)
    }
    bondContractDetails <- function(curMsg, msg, timestamp,
                                    file, ...) {
      c(curMsg, msg)
    }
    contractDetailsEnd <- function(curMsg, msg, timestamp,
                                   file, ...) {
      c(curMsg, msg)
    }
    execDetails <- function(curMsg, msg, timestamp, file,
                            ...) {
      e_execDetails(curMsg, msg, file, ...)
    }
    execDetailsEnd <- function(curMsg, msg, timestamp, file,
                               ...) {
      c(curMsg, msg)
    }
    updateMktDepth <- function(curMsg, msg, timestamp, file,
                               ...) {
      symbols <- get.Data("symbols")
      e_update_mkt_depth(NULL, msg, timestamp, file, symbols,
                         ...)
    }
    updateMktDepthL2 <- function(curMsg, msg, timestamp,
                                 file, ...) {
      symbols <- get.Data("symbols")
      e_update_mkt_depthL2(NULL, msg, timestamp, file,
                           symbols, ...)
    }
    updateNewsBulletin <- function(curMsg, msg, timestamp,
                                   file, ...) {
      cat("newsMsgId: ", msg[2], "newsMsgType: ", msg[3],
          "newsMessage: ", msg[4], "origExch:", msg[5],
          "\n")
      c(curMsg, msg)
    }
    managedAccounts <- function(curMsg, msg, timestamp, file,
                                ...) {
      c(curMsg, msg)
    }
    receiveFA <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    historicalData <- function(curMsg, msg, timestamp, file,
                               ...) {
      c(curMsg, msg)
    }
    scannerParameters <- function(curMsg, msg, timestamp,
                                  file, ...) {
      cat(msg <- rawToChar(msg[-which(msg == as.raw(0))]))
      c(curMsg, msg)
    }
    scannerData <- function(curMsg, reqId, rank, contract,
                            distance, benchmark, projection, legsStr) {
      e_scannerData(curMsg, reqId, rank, contract, distance,
                    benchmark, projection, legsStr)
    }
    scannerDataEnd <- function(curMsg, msg, timestamp, file,
                               ...) {
      c(curMsg, msg)
    }
    realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- get.Data("symbols")
      e_real_time_bars(curMsg, msg, symbols, file, ...)
    }
    currentTime <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    fundamentalData <- function(curMsg, msg, timestamp, file,
                                ...) {
      e_fundamentalData(curMsg, msg)
    }
    deltaNeutralValidation <- function(curMsg, msg, timestamp,
                                       file, ...) {
      c(curMsg, msg)
    }
    tickSnapshotEnd <- function(curMsg, msg, timestamp, file,
                                ...) {
      c(curMsg, msg)
    }
  }
  else {
    tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          msg, timestamp, file, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n", file=file[[1]],
          append=TRUE, ...)
    }
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n", file=file[[1]],
          append=TRUE, ...)
    }
  }
  ew_env <- list(.Data=.Data, get.Data=get.Data, assign.Data=assign.Data,
                 remove.Data=remove.Data, tickPrice=tickPrice, tickSize=tickSize,
                 tickOptionComputation=tickOptionComputation, tickGeneric=tickGeneric,
                 tickString=tickString, tickEFP=tickEFP, orderStatus=orderStatus,
                 errorMessage=errorMessage, openOrder=openOrder, openOrderEnd=openOrderEnd,
                 updateAccountValue=updateAccountValue, updatePortfolio=updatePortfolio,
                 updateAccountTime=updateAccountTime, accountDownloadEnd=accountDownloadEnd,
                 nextValidId=nextValidId, contractDetails=contractDetails,
                 bondContractDetails=bondContractDetails, contractDetailsEnd=contractDetailsEnd,
                 execDetails=execDetails, execDetailsEnd=execDetailsEnd,
                 updateMktDepth=updateMktDepth, updateMktDepthL2=updateMktDepthL2,
                 updateNewsBulletin=updateNewsBulletin, managedAccounts=managedAccounts,
                 receiveFA=receiveFA, historicalData=historicalData,
                 scannerParameters=scannerParameters, scannerData=scannerData,
                 scannerDataEnd=scannerDataEnd, realtimeBars=realtimeBars,
                 currentTime=currentTime, fundamentalData=fundamentalData,
                 deltaNeutralValidation=deltaNeutralValidation, tickSnapshotEnd=tickSnapshotEnd)
  class(ew_env) <- "eWrapper"
  invisible(ew_env)
}  # end create_ewrapper


