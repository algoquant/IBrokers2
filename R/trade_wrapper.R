# n_contracts is the number of contracts in the data buffer
trade_wrapper <- function(con_tracts=NULL, limit_prices=NULL, file_connects, lamb_da) {
  # cat("Entering trade_wrapper", "\n")
  # Create eWrapper environment
  e_wrapper <- create_ewrapper(NULL)
  e_wrapper$lamb_da <- lamb_da
  # e_wrapper <- new.env()
  # Create eWrapper accessor functions
  # e_wrapper$get_ <- function(x) get(x, e_wrapper)
  # e_wrapper$assign_ <- function(x, value) assign(x, value, e_wrapper)
  # e_wrapper$remove_ <- function(x) remove(x, e_wrapper)
  # Initialize state variables in eWrapper environment
  # e_wrapper$assign.Data("count_er", 0)
  # Define dimensions of data buffer for single instrument
  col_names <- c("Time", "Open", "High", "Low", "Close", "Volume", "WAP", "Count")
  # e_wrapper$assign.Data("col_names", col_names)
  e_wrapper$col_names <- col_names
  n_row <- 8*60*12; n_col <- NROW(col_names)
  e_wrapper$n_row <- n_row
  e_wrapper$n_col <- n_col
  # e_wrapper$n_row <- 8*60*12; e_wrapper$n_col <- NROW(e_wrapper$col_names)
  if (is.null(con_tracts))
    stop("con_tracts argument is missing")
  else {
    e_wrapper$con_tracts <- con_tracts
    n_contracts <- NROW(con_tracts)
    e_wrapper$n_contracts <- n_contracts
    # count_er for counting number of bars per contract
    e_wrapper$count_er <- integer(n_contracts)
    e_wrapper$vols <- numeric(n_contracts)
    e_wrapper$beta <- numeric(1)
    name_s <- names(con_tracts)
    e_wrapper$name_s <- name_s
  }  # end if
  if (is.null(limit_prices))
    stop("limit_prices argument is missing")
  else
    e_wrapper$limit_prices <- limit_prices

  # Create data buffer bar_data, as a list of matrices in the eWrapper environment
  e_wrapper$bar_data <- rep(list(matrix(rep(NA_real_, n_row*n_col), ncol=n_col)), n_contracts)
  names(e_wrapper$bar_data) <- e_wrapper$name_s
  for (it in 1:NROW(name_s)) {
    col_n <- paste(name_s[it], col_names, sep=".")
    colnames(e_wrapper$bar_data[[it]]) <- col_n
    # Write headers to data files
    cat(paste(col_n, collapse=","), "\n", file=file_connects[[it]], append=TRUE)
  }  # end for
  # Create trade_ids buffer, as a matrix of trade ids in the eWrapper environment
  e_wrapper$trade_ids <- matrix(rep(NA_integer_, 2*n_contracts), ncol=2)
  colnames(e_wrapper$trade_ids) <- c("buy_id", "sell_id")
  # names(e_wrapper$trade_ids) <- e_wrapper$name_s

  # Create data buffer bar_data, as a list of data frames in the eWrapper environment
  # e_wrapper$bar_data <- rep(list(as.data.frame(matrix(rep(NA_real_, n_row*n_col), ncol=n_col))), n_contracts)
  # Create data buffer bar_data, as a list of xts series in the eWrapper environment
  # e_wrapper$bar_data <- rep(list(structure(.xts(matrix(rep(NA_real_, n_row*n_col), ncol=n_col), 1:n_row),
  #                                       .Dimnames=list(NULL, col_names))),
  #                        n_contracts)

  # Initialize trading model parameters
  # e_wrapper$buy_spread <- buy_spread
  # e_wrapper$sell_spread <- sell_spread
  # Initialize state variables in eWrapper environment
  # e_wrapper$buy_id <- 0
  # e_wrapper$sell_id <- 0

  ## Define trading model function inside the eWrapper environment
  # The function model_fun is called from inside realtimeBars()
  e_wrapper$model_fun <- function(new_bar, contract_id) {
    # if (!IBrokers2::isConnected(ib_connect)) {ib_connect <- IBrokers2::twsConnect(port=7497) ; cat("reconnected")}
    # cat("model_fun: ", e_wrapper$count_er, "\n")
    # Cancel previous trade orders
    if (e_wrapper$count_er[contract_id] > 1) {
      IBrokers2::cancelOrder(ib_connect, e_wrapper$trade_ids[contract_id, "buy_id"])
      IBrokers2::cancelOrder(ib_connect, e_wrapper$trade_ids[contract_id, "sell_id"])
    }  # end if

    limit_prices <- e_wrapper$limit_prices[[contract_id]]
    # Execute buy limit order
    buy_id <- IBrokers2::reqIds(ib_connect)
    buy_price <- (new_bar["Low"] - limit_prices["buy_spread"])
    buy_order <- IBrokers2::twsOrder(buy_id, orderType="LMT",
                                     lmtPrice=buy_price, action="BUY", totalQuantity=1)
    IBrokers2::placeOrder(ib_connect, e_wrapper$con_tracts[[contract_id]], buy_order)

    # Execute sell limit order
    sell_id <- IBrokers2::reqIds(ib_connect)
    sell_price <- (new_bar["High"] + limit_prices["sell_spread"])
    sell_order <- IBrokers2::twsOrder(sell_id, orderType="LMT",
                                      lmtPrice=sell_price, action="SELL", totalQuantity=1)
    IBrokers2::placeOrder(ib_connect, e_wrapper$con_tracts[[contract_id]], sell_order)

    e_wrapper$trade_ids[contract_id, "buy_id"] <<- buy_id
    e_wrapper$trade_ids[contract_id, "sell_id"] <<- sell_id

    cat("Buy order at: ", buy_price, "\tSell order at: ", sell_price, "\n")

    invisible(list(buy_id=buy_id, sell_id=sell_id))
  }  # end model_fun


  # realtimeBars() processes a new bar of data and runs the model_fun()
  # realtimeBars() is called by processMsg() in a callback loop inside twsCALLBACK()
  e_wrapper$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    # Unwrap new bar of data in msg passed from processMsg()
    new_bar <- as.numeric(msg)
    # cat("realtimeBars col_names: ", col_names, "\n")
    # cat("realtimeBars n_col: ", n_col, "\n")
    col_index <- (3:(e_wrapper$n_col+2))
    names(new_bar)[col_index] <- e_wrapper$col_names
    contract_id <- new_bar[2]
    # cat("realtimeBars count_er: ", e_wrapper$count_er, "\n")
    # e_wrapper$assign.Data("count_er", e_wrapper$get.Data("count_er")+1)
    count_er <- e_wrapper$count_er[contract_id] + 1
    e_wrapper$count_er[contract_id] <<- count_er
    # cat("realtimeBars new_bar: ", new_bar, "\n")
    # cat("realtimeBars: ", e_wrapper$get.Data("count_er"), "\n")
    # Copy new bar of data into buffer
    e_wrapper$bar_data[[contract_id]][count_er, ] <<- new_bar[col_index]
    # cat("realtimeBars: ", e_wrapper$name_s[contract_id], " vol: ", e_wrapper$vols[contract_id], "\n")
    e_wrapper$vols[contract_id] <<- e_wrapper$lamb_da*(new_bar["High"]-new_bar["Low"]) + as.numeric(count_er>1)*(1-e_wrapper$lamb_da)*e_wrapper$vols[contract_id]
    # star_t <- max(2, count_er-10)
    # e_wrapper$beta <<-
    # cat("realtimeBars bar_data: ", e_wrapper$bar_data[[contract_id]][e_wrapper$count_er, ], "\n")
    # if (e_wrapper$count_er > 1)
    #   cat("realtimeBars bar_data: ", e_wrapper$bar_data[[contract_id]][e_wrapper$count_er-1, ], "\n")
    # if (e_wrapper$count_er > 2)
    #   cat("realtimeBars bar_data: ", e_wrapper$bar_data[[contract_id]][e_wrapper$count_er-2, ], "\n")
    # Write to file
    # file_name <- file[[contract_id]]
    cat(paste(e_wrapper$bar_data[[contract_id]][count_er, ], collapse=","), "\n", file=file[[contract_id]], append=TRUE)
    # Write to file and add contract_id
    # cat(paste(e_wrapper$name_s[[contract_id]], paste(e_wrapper$bar_data[[contract_id]][e_wrapper$count_er, ], collapse=","), sep=","), "\n",
    #     file=file[[contract_id]], append=TRUE)
    # Write to file every 10 counts
    # if ((e_wrapper$count_er %% 10) == 0) {
    #   for (contract_id in 1:(e_wrapper$n_contracts)) {
    #     data.table::fwrite(e_wrapper$bar_data[[e_wrapper$contract_id]], file=file[[e_wrapper$contract_id]])
    #   }  # end for
    # }  # end if
    # cat(c(e_wrapper$name_s[[contract_id]], paste(e_wrapper$bar_data[[contract_id]][e_wrapper$count_er, ], collapse=",")), "\n")
    # Write to console
    cat(paste0("count_er=", count_er), paste0("vol=", e_wrapper$vols[contract_id]), paste0(colnames(e_wrapper$bar_data[[contract_id]]), "=", e_wrapper$bar_data[[contract_id]][count_er, ]), "\n")
    # cat("Number of rows of data for instrument ", contract_id, " is = ", NROW(e_wrapper$bar_data[[e_wrapper$contract_id]]), "\n")
    # cat(paste0("Open=", new_bar[4], "\tHigh=", new_bar[5], "\tLow=", new_bar[6], "\tClose=", new_bar[7], "\tVolume=", new_bar[8]), "\n")
    # Run the trading model
    if (!is.na(e_wrapper$limit_prices[[contract_id]]))
      e_wrapper$model_fun(new_bar, contract_id)
    # Return values
    c(curMsg, msg)
  }  # end realtimeBars

  return(e_wrapper)
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
  e_wrapper <- list(.Data=.Data, get.Data=get.Data, assign.Data=assign.Data,
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
  class(e_wrapper) <- "eWrapper"
  invisible(e_wrapper)
}  # end create_ewrapper


