#' Create a wrapper environment and define the function realtimeBars() for
#' running systematic trading strategies in a callback loop.
#'
#' @details The function \code{trade_wrapper()} is derived from
#'   \code{IBrokers::eWrapper.RealTimeBars.CSV()}. It creates a wrapper
#'   environment and defines the function realtimeBars() for running systematic
#'   trading strategies in a callback loop.
#'
#' @export
trade_wrapper <- function (n = 1) {
  eW <- eWrapper_new(NULL)
  # eW <- IBrokers::eWrapper(NULL)
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count")))), n))
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, buy_spread=0.25, sell_spread=0.25, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    # write to file
    cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
    # write to console
    # eW$count_er <- eW$count_er + 1
    eW$assign.Data("count_er", eW$get.Data("count_er")+1)
    cat(paste0("count_er=", eW$get.Data("count_er"), "\tOpen=", msg[4], "\tHigh=", msg[5], "\tLow=", msg[6], "\tClose=", msg[7], "\tVolume=", msg[8]), "\n")
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
    buy_id <- as.numeric(eW$get.Data("buy_id"))
    sell_id <- as.numeric(eW$get.Data("sell_id"))
    if (buy_id>0) IBrokers::cancelOrder(ib_connect, buy_id)
    if (sell_id>0) IBrokers::cancelOrder(ib_connect, sell_id)
    # Execute buy limit order
    buy_id <- as.numeric(IBrokers::reqIds(ib_connect))
    buy_order <- IBrokers::twsOrder(buy_id, orderType="LMT",
                                    lmtPrice=(as.numeric(msg[6])-buy_spread), action="BUY", totalQuantity=1)
    IBrokers::placeOrder(ib_connect, con_tract, buy_order)
    # Execute sell limit order
    sell_id <- as.numeric(IBrokers::reqIds(ib_connect))
    # if (!IBrokers::isConnected(ib_connect)) {ib_connect <- IBrokers::twsConnect(port=7497) ; cat("reconnected")}
    sell_order <- IBrokers::twsOrder(sell_id, orderType="LMT",
                                     lmtPrice=(as.numeric(msg[5])+sell_spread), action="SELL", totalQuantity=1)
    # if (!IBrokers::isConnected(ib_connect)) {ib_connect <- IBrokers::twsConnect(port=7497) ; cat("reconnected")}
    IBrokers::placeOrder(ib_connect, con_tract, sell_order)
    # Copy new trade orders
    # cat(paste0("buy_id=", buy_id, "\tsell_id=", sell_id), "\n")
    eW$assign.Data("buy_id", buy_id)
    eW$assign.Data("sell_id", sell_id)
    ### Trade code end

    data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }  # end eW$realtimeBars
  return(eW)
}  # end trade_wrapper


#' @export
eWrapper_new <- function (debug = FALSE, errfile = stderr()) {
  .Data <- new.env()
  get.Data <- function(x) get(x, .Data)
  assign.Data <- function(x, value) assign(x, value, .Data)
  remove.Data <- function(x) remove(x, .Data)
  # count_er <- 0
  assign.Data("count_er", 0)
  assign.Data("buy_id", 0)
  assign.Data("sell_id", 0)
  if (is.null(debug)) {
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      cat(msg, "\n", file = errfile)
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
      cat(as.character(timestamp), curMsg, msg, "\n", file = file[[1]],
          append = TRUE, ...)
    }
    errorMessage <- function(curMsg, msg, timestamp, file,
                             twsconn, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n", file = file[[1]],
          append = TRUE, ...)
    }
  }
  eW <- list(.Data = .Data, get.Data = get.Data, assign.Data = assign.Data,
             remove.Data = remove.Data, tickPrice = tickPrice, tickSize = tickSize,
             tickOptionComputation = tickOptionComputation, tickGeneric = tickGeneric,
             tickString = tickString, tickEFP = tickEFP, orderStatus = orderStatus,
             errorMessage = errorMessage, openOrder = openOrder, openOrderEnd = openOrderEnd,
             updateAccountValue = updateAccountValue, updatePortfolio = updatePortfolio,
             updateAccountTime = updateAccountTime, accountDownloadEnd = accountDownloadEnd,
             nextValidId = nextValidId, contractDetails = contractDetails,
             bondContractDetails = bondContractDetails, contractDetailsEnd = contractDetailsEnd,
             execDetails = execDetails, execDetailsEnd = execDetailsEnd,
             updateMktDepth = updateMktDepth, updateMktDepthL2 = updateMktDepthL2,
             updateNewsBulletin = updateNewsBulletin, managedAccounts = managedAccounts,
             receiveFA = receiveFA, historicalData = historicalData,
             scannerParameters = scannerParameters, scannerData = scannerData,
             scannerDataEnd = scannerDataEnd, realtimeBars = realtimeBars,
             currentTime = currentTime, fundamentalData = fundamentalData,
             deltaNeutralValidation = deltaNeutralValidation, tickSnapshotEnd = tickSnapshotEnd)
  class(eW) <- "eWrapper"
  invisible(eW)
}  # end eWrapper_new


