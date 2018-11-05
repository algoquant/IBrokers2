
trade_wrapper_old <- function(n_instr=1, ...) {
  ew_env <- eWrapper_new(NULL)
  # ew_env <- IBrokers::eWrapper(NULL)
  ew_env$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n_instr))
  ew_env$assign.Data("count_er", 0)

  # Unpack the dots containing the trading model parameters
  ew_env$model_params <- list(...)
  # If dots are empty then set default parameter values
  if (NROW(ew_env$model_params) == 0) {
    ew_env$model_params$buy_spread <- 0.25
    ew_env$model_params$sell_spread <- 0.25
  }  # end if

  ew_env$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- ew_env$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nrew_env <- NROW(data[[id]])
    # write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # write to console
    # ew_env$count_er <- ew_env$count_er + 1
    ew_env$assign.Data("count_er", ew_env$get.Data("count_er")+1)
    cat(paste0("count_er=", ew_env$get.Data("count_er"), "\tOpen=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
    # cat(paste0("Open=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")

    ### Trade code start
    # cat(paste0("Available Funds=", IBrokers::reqAccountUpdates(conn=ib_connect, acctCode="DI1207807")[[1]]$AvailableFunds[1]), "\n")
    # if (rnorm(1) > 0) {
    #   order_id <- IBrokers::reqIds(ib_connect)
    #   ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
    #                                  action="SELL", totalQuantity="1")
    #   IBrokers::placeOrder(ib_connect, con_tract, ib_order)
    # } else {
    #   order_id <- IBrokers::reqIds(ib_connect)
    #   ib_order <- IBrokers::twsOrder(order_id, orderType="MKT",
    #                                  action="BUY", totalQuantity="1")
    #   IBrokers::placeOrder(ib_connect, con_tract, ib_order)
    # }  # end if
    ### Trade code end

    ### Trade code start
    # Cancel previous trade orders
    # if (!IBrokers::isConnected(ib_connect)) {ib_connect <- IBrokers::twsConnect(port=7497) ; cat("reconnected")}
    buy_id <- as.numeric(ew_env$get.Data("buy_id"))
    sell_id <- as.numeric(ew_env$get.Data("sell_id"))
    if (buy_id>0) IBrokers::cancelOrder(ib_connect, buy_id)
    if (sell_id>0) IBrokers::cancelOrder(ib_connect, sell_id)
    # Execute buy limit order
    buy_id <- as.numeric(IBrokers::reqIds(ib_connect))
    buy_order <- IBrokers::twsOrder(buy_id, orderType="LMT",
                                    lmtPrice=(as.numeric(msg[6])-ew_env$model_params$buy_spread), action="BUY", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, buy_order)
    # Execute sell limit order
    sell_id <- as.numeric(IBrokers::reqIds(ib_connect))
    # if (!IBrokers::isConnected(ib_connect)) {ib_connect <- IBrokers::twsConnect(port=7497) ; cat("reconnected")}
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                                     lmtPrice=(as.numeric(msg[5])+ew_env$model_params$sell_spread), action="SELL", totalQuantity=1)
    # if (!IBrokers::isConnected(ib_connect)) {ib_connect <- IBrokers::twsConnect(port=7497) ; cat("reconnected")}
    IBrokers::placeOrder(ib_connect, con_tract, sell_order)
    # Copy new trade orders
    # cat(paste0("buy_id=", buy_id, "\tsell_id=", sell_id), "\n")
    ew_env$assign.Data("buy_id", buy_id)
    ew_env$assign.Data("sell_id", sell_id)
    ### Trade code end

    data[[id]][nrew_env, 1:7] <- as.numeric(msg[4:10])
    ew_env$assign.Data("data", data)
    c(curMsg, msg)
  }  # end ew_env$realtimeBars
  return(ew_env)
}  # end trade_wrapper_old


# n_instr is the number of instruments in the data buffer
trade_wrapper <- function(n_instr=1, ...) {
  # cat("Entering trade_wrapper", "\n")
  # Create eWrapper environment
  ew_env <- create_ewrapper(NULL)
  # ew_env <- new.env()
  # Create eWrapper accessor functions
  # ew_env$get_ <- function(x) get(x, ew_env)
  # ew_env$assign_ <- function(x, value) assign(x, value, ew_env)
  # ew_env$remove_ <- function(x) remove(x, ew_env)
  # Initialize state variables in eWrapper environment
  ew_env$assign.Data("count_er", 0)
  # ew_env$count_er <- 0
  # Define dimensions of data buffer for single instrument
  ew_env$col_names <- c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")
  ew_env$n_row <- 8*60*12; ew_env$n_col <- NROW(ew_env$col_names)
  # Create data buffer bar_data, as a list of xts series in eWrapper environment
  ew_env$bar_data <- with(ew_env, rep(list(structure(.xts(matrix(rep(NA_real_, n_row*n_col), ncol=n_col), 1:n_row),
                                                     .Dimnames=list(NULL, col_names))),
                                      n_instr))

  ## Define trading model function inside the eWrapper environment
  # Unpack the dots containing the trading model parameters
  ew_env$model_params <- list(...)
  # If dots are empty then set default parameter values
  if (NROW(ew_env$model_params) == 0) {
    ew_env$model_params$buy_spread <- 0.25
    ew_env$model_params$sell_spread <- 0.25
  }  # end if
  # Initialize state variables in eWrapper environment
  ew_env$buy_id <- 0
  ew_env$sell_id <- 0

  # The function model_fun is called from inside realtimeBars()
  # cat("Defining model_fun", "\n")
  ew_env$model_fun <- function(new_bar) {
    # if (!IBrokers2::isConnected(ib_connect)) {ib_connect <- IBrokers2::twsConnect(port=7497) ; cat("reconnected")}
    # Cancel previous trade orders
    # ew_env$count_er <- 3
    # cat("model_fun: ", ew_env$get.Data("count_er"), "\n")
    if (ew_env$get.Data("count_er") > 1) {
      IBrokers2::cancelOrder(ib_connect, ew_env$get.Data("buy_id"))
      IBrokers2::cancelOrder(ib_connect, ew_env$get.Data("sell_id"))
    }  # end if

    # Execute buy limit order
    buy_id <- IBrokers2::reqIds(ib_connect)
    buy_price <- (new_bar["Low"] - ew_env$model_params$buy_spread)
    buy_order <- IBrokers2::twsOrder(buy_id, orderType="LMT",
                                     lmtPrice=buy_price, action="BUY", totalQuantity=1)
    IBrokers2::placeOrder(ib_connect, con_tract, buy_order)

    # Execute sell limit order
    sell_id <- IBrokers2::reqIds(ib_connect)
    sell_price <- (new_bar["High"] + ew_env$model_params$sell_spread)
    sell_order <- IBrokers2::twsOrder(sell_id, orderType="LMT",
                                      lmtPrice=sell_price, action="SELL", totalQuantity=1)
    IBrokers2::placeOrder(ib_connect, con_tract, sell_order)

    # cat("Buy order at: ", buy_price, "\tSell order at: ", sell_price, "\n")

    # Copy new trade orders
    ew_env$assign.Data("buy_id", buy_id)
    ew_env$assign.Data("sell_id", sell_id)
    # ew_env$buy_id <- buy_id
    # ew_env$sell_id <- sell_id
    invisible(c(buy_order=buy_price, sell_order=sell_price))
  }  # end model_fun

  # realtimeBars() processes a new bar of data and runs the model_fun()
  # realtimeBars() is called by processMsg() in a callback loop inside twsCALLBACK()
  ew_env$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    # Unwrap new bar of data in msg passed from processMsg()
    ew_env$assign.Data("count_er", ew_env$get.Data("count_er")+1)
    # ew_env$assign_("count_er", ew_env$get_("count_er")+1)
    # ew_env$count_er <- ew_env$count_er + 1
    # cat("realtimeBars: ", ew_env$get.Data("count_er"), "\n")
    new_bar <- as.numeric(msg)
    names(new_bar)[4:(ew_env$n_col+3)] <- ew_env$col_names
    # cat("realtimeBars col_names: ", ew_env$col_names, "\n")
    # cat("realtimeBars new_bar: ", new_bar, "\n")
    instr_id <- new_bar[2]
    # Copy new bar of data into buffer
    ew_env$bar_data[[instr_id]][ew_env$get.Data("count_er"), ] <- new_bar[4:(ew_env$n_col+3)]
    # cat("realtimeBars bar_data: ", head(ew_env$bar_data[[instr_id]]), "\n")
    # Write to file
    # file_name <- file[[instr_id]]
    # cat(paste(new_bar[3], new_bar[4], new_bar[5], new_bar[6], new_bar[7], new_bar[8], new_bar[9], new_bar[10], sep=","), "\n", file=file, append=TRUE)
    # Write to console
    cat(paste0("count_er=", ew_env$get.Data("count_er")), paste0(ew_env$col_names, "=", new_bar[4:(ew_env$n_col+3)]), "\n")
    # cat(paste0("Open=", new_bar[4], "\tHigh=", new_bar[5], "\tLow=", new_bar[6], "\tClose=", new_bar[7], "\tVolume=", new_bar[8]), "\n")
    # Run the trading model
    ew_env$model_fun(new_bar)
    # Return values
    c(curMsg, msg)
  }  # end ew_env$realtimeBars
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


