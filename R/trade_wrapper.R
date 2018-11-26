# n_contracts is the number of contracts in the data buffer
trade_wrapper <- function(con_tracts=NULL,
                          trade_params=NULL,
                          file_connects,
                          warm_up=100) { # warmup period
  # cat("Entering trade_wrapper", "\n")
  # Create eWrapper environment
  e_wrapper <- create_ewrapper(NULL)
  e_wrapper$da_ta$warm_up <- warm_up
  # e_wrapper <- new.env()
  # Create eWrapper accessor functions
  # e_wrapper$get_ <- function(x) get(x, e_wrapper)
  # e_wrapper$assign_ <- function(x, value) assign(x, value, e_wrapper)
  # e_wrapper$remove_ <- function(x) remove(x, e_wrapper)
  # Initialize state variables in eWrapper environment
  # e_wrapper$as_sign("count_er", 0)
  # Define dimensions of data buffer for single instrument
  col_names <- c("Time", "Open", "High", "Low", "Close", "Volume", "WAP", "Count")
  # e_wrapper$as_sign("col_names", col_names)
  e_wrapper$da_ta$col_names <- col_names
  n_rows <- 8*60*12; n_cols <- NROW(col_names)
  e_wrapper$da_ta$n_rows <- n_rows
  e_wrapper$da_ta$n_cols <- n_cols
  if (is.null(con_tracts))
    stop("con_tracts argument is missing")
  else {
    e_wrapper$da_ta$con_tracts <- con_tracts
    n_contracts <- NROW(con_tracts)
    e_wrapper$da_ta$n_contracts <- n_contracts
    name_s <- names(con_tracts)
    e_wrapper$da_ta$name_s <- name_s
    # count_er for counting number of bars collected for each contract
    e_wrapper$da_ta$count_er <- integer(n_contracts)
    # Vector of volatilities, one for each contract
    e_wrapper$da_ta$vols <- numeric(n_contracts)
    # Vector of EWMAs, one for each contract
    e_wrapper$da_ta$ewmas <- numeric(n_contracts)
    # Vector of net positions, one for each contract
    e_wrapper$da_ta$position <- integer(n_contracts)
    # e_wrapper$da_ta$beta <- numeric(1)
  }  # end if
  if (is.null(trade_params))
    stop("trade_params argument is missing")
  else {
    if (!(n_contracts == NROW(trade_params)))
      stop("trade_params argument is missing some elements")
    e_wrapper$da_ta$trade_params <- trade_params
  }  # end if

  # Create data buffer bar_data, a list of matrices in the eWrapper environment
  e_wrapper$da_ta$bar_data <- rep(list(matrix(rep(NA_real_, n_rows*n_cols), ncol=n_cols)), n_contracts)
  names(e_wrapper$da_ta$bar_data) <- name_s
  for (it in 1:NROW(name_s)) {
    col_n <- paste(name_s[it], col_names, sep=".")
    colnames(e_wrapper$da_ta$bar_data[[it]]) <- col_n
    # Write headers to data files
    cat(paste(col_n, collapse=","), "\n", file=file_connects[[it]], append=TRUE)
  }  # end for
  # Create trade_ids buffer, as a matrix of trade ids in the eWrapper environment
  e_wrapper$da_ta$trade_ids <- matrix(rep(NA_integer_, 2*n_contracts), ncol=2)
  colnames(e_wrapper$da_ta$trade_ids) <- c("buy_id", "sell_id")
  # names(e_wrapper$da_ta$trade_ids) <- e_wrapper$da_ta$name_s

  # Create data buffer bar_data, as a list of data frames in the eWrapper environment
  # e_wrapper$da_ta$bar_data <- rep(list(as.data.frame(matrix(rep(NA_real_, n_rows*n_cols), ncol=n_cols))), n_contracts)
  # Create data buffer bar_data, as a list of xts series in the eWrapper environment
  # e_wrapper$da_ta$bar_data <- rep(list(structure(.xts(matrix(rep(NA_real_, n_rows*n_cols), ncol=n_cols), 1:n_rows),
  #                                       .Dimnames=list(NULL, col_names))),
  #                        n_contracts)

  # Initialize trading model parameters
  # e_wrapper$da_ta$buy_spread <- buy_spread
  # e_wrapper$da_ta$sell_spread <- sell_spread
  # Initialize state variables in eWrapper environment
  # e_wrapper$da_ta$buy_id <- 0
  # e_wrapper$da_ta$sell_id <- 0

  ## Define trading model function inside the eWrapper environment
  # The function model_fun is called from inside realtimeBars()
  e_wrapper$model_fun <- function(contract_id, trade_params, ib_connect) {
    # if (!IBrokers2::isConnected(ib_connect)) {ib_connect <- IBrokers2::twsConnect(port=7497) ; cat("reconnected")}
    # cat("model_fun count_er: ", e_wrapper$da_ta$count_er, "\n")
    # Cancel previous trade orders
    if (IBrokers2::is.twsConnection(ib_connect) &&
        !is.na(e_wrapper$da_ta$trade_ids[contract_id, "buy_id"]) &&
         !is.na(e_wrapper$da_ta$trade_ids[contract_id, "sell_id"])) {
      IBrokers2::cancelOrder(ib_connect, e_wrapper$da_ta$trade_ids[contract_id, "buy_id"])
      IBrokers2::cancelOrder(ib_connect, e_wrapper$da_ta$trade_ids[contract_id, "sell_id"])
    }  # end if

    lagg <- trade_params["lagg"]
    # trade_params <- e_wrapper$da_ta$trade_params[[contract_id]]
    # scale the sprea_d by the vol level - default is zero
    # sprea_d <- 0.25*trunc(e_wrapper$da_ta$fac_tor*e_wrapper$da_ta$vols[contract_id])
    # Calculate sprea_d by comparing lagged WAP with EWMA
    count_er <- e_wrapper$da_ta$count_er[contract_id]
    it <- (count_er - lagg)
    ohlc_lag <- e_wrapper$da_ta$bar_data[[contract_id]][it, ]
    sprea_d <- (if (ohlc_lag[7] > e_wrapper$da_ta$ewmas[contract_id]) 0.25 else -0.25)
    # Limit prices are the low and high prices of the lagged bar, plus the spreads
    buy_limit <- (ohlc_lag[4] - trade_params["buy_spread"] + sprea_d)
    sell_limit <- (ohlc_lag[3] + trade_params["sell_spread"] + sprea_d)

    # buy_limit should be no greater than close price
    clo_se <- e_wrapper$da_ta$bar_data[[contract_id]][count_er, 5]
    buy_limit <- min(clo_se, buy_limit)
    # sell_limit should be no less than close price
    sell_limit <- max(clo_se, sell_limit)

    # cat("model_fun trade_params: ", trade_params, "\n")
    if (IBrokers2::is.twsConnection(ib_connect)) {
      # Execute buy limit order
      buy_id <- IBrokers2::reqIds(ib_connect)
      buy_order <- IBrokers2::twsOrder(buy_id, orderType="LMT",
                                       lmtPrice=buy_limit, action="BUY", totalQuantity=trade_params["siz_e"])
      IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], buy_order)

      # Execute sell limit order
      sell_id <- IBrokers2::reqIds(ib_connect)
      sell_order <- IBrokers2::twsOrder(sell_id, orderType="LMT",
                                        lmtPrice=sell_limit, action="SELL", totalQuantity=trade_params["siz_e"])
      IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], sell_order)

      e_wrapper$da_ta$trade_ids[contract_id, "buy_id"] <<- buy_id
      e_wrapper$da_ta$trade_ids[contract_id, "sell_id"] <<- sell_id
    } else {
      # Write to file
      cat("Buy limit:,", buy_limit, ",Sell limit:,", sell_limit, "\n", file=ib_connect, append=TRUE)
    }  # end if
    # cat("Buy limit order at: ", buy_limit, "\tSell limit order at: ", sell_limit, "\n")

    invisible(list(buy_limit=buy_limit, sell_limit=sell_limit))
  }  # end model_fun


  # realtimeBars() processes a new bar of data and runs the model_fun()
  # realtimeBars() is called by processMsg() in a callback loop inside twsCALLBACK()
  e_wrapper$realtimeBars <- function(curMsg, msg, timestamp, file, ib_connect, ...) {
    # Unwrap new bar of data in msg passed from processMsg()
    new_bar <- as.numeric(msg)
    # cat("realtimeBars col_names: ", col_names, "\n")
    # cat("realtimeBars n_cols: ", n_cols, "\n")
    col_index <- (3:(e_wrapper$da_ta$n_cols+2))
    names(new_bar)[col_index] <- e_wrapper$da_ta$col_names
    contract_id <- new_bar[2]
    trade_params <- e_wrapper$da_ta$trade_params[[contract_id]]
    # cat("realtimeBars count_er: ", e_wrapper$da_ta$count_er, "\n")
    # e_wrapper$as_sign("count_er", e_wrapper$ge_t("count_er")+1)
    count_er <- e_wrapper$da_ta$count_er[contract_id] + 1
    e_wrapper$da_ta$count_er[contract_id] <- count_er
    # assign("count_er", count_er, e_wrapper$da_ta)
    # e_wrapper$da_ta$count_er[contract_id] <<- count_er
    # cat("realtimeBars new_bar: ", new_bar, "\n")
    # cat("realtimeBars: ", e_wrapper$ge_t("count_er"), "\n")
    # Copy new bar of data into buffer
    e_wrapper$da_ta$bar_data[[contract_id]][count_er, ] <<- new_bar[col_index]
    # cat("realtimeBars: ", e_wrapper$da_ta$name_s[contract_id], " vol: ", e_wrapper$da_ta$vols[contract_id], "\n")
    if (!is.na(trade_params)) {
      if (count_er>1) {lamb_da <- trade_params["lamb_da"]} else {lamb_da <- 1}
      e_wrapper$da_ta$vols[contract_id] <<- lamb_da*(new_bar["High"]-new_bar["Low"]) + (1-lamb_da)*e_wrapper$da_ta$vols[contract_id]
      e_wrapper$da_ta$ewmas[contract_id] <<- lamb_da*new_bar["WAP"] + (1-lamb_da)*e_wrapper$da_ta$ewmas[contract_id]

      # wipp Download net position from IB
      ib_account <- IBrokers2::reqAccountUpdates(conn=ib_connect, acctCode="DI1207807")
      e_wrapper$da_ta$position[contract_id] <- unlist(ib_account[[2]])["portfolioValue.position"]
      cat("Net position from IB: ", e_wrapper$da_ta$position[contract_id], "\n")

    }  # end if
    # star_t <- max(2, count_er-10)
    # e_wrapper$da_ta$beta <<-
    # cat("realtimeBars bar_data: ", e_wrapper$da_ta$bar_data[[contract_id]][e_wrapper$da_ta$count_er, ], "\n")
    # if (e_wrapper$da_ta$count_er > 1)
    #   cat("realtimeBars bar_data: ", e_wrapper$da_ta$bar_data[[contract_id]][e_wrapper$da_ta$count_er-1, ], "\n")
    # if (e_wrapper$da_ta$count_er > 2)
    #   cat("realtimeBars bar_data: ", e_wrapper$da_ta$bar_data[[contract_id]][e_wrapper$da_ta$count_er-2, ], "\n")
    # Write to file
    # file_name <- file[[contract_id]]
    cat(paste(e_wrapper$da_ta$bar_data[[contract_id]][count_er, ], collapse=","), "\n", file=file[[contract_id]], append=TRUE)
    # Write to file and add contract_id
    # cat(paste(e_wrapper$da_ta$name_s[[contract_id]], paste(e_wrapper$da_ta$bar_data[[contract_id]][e_wrapper$da_ta$count_er, ], collapse=","), sep=","), "\n",
    #     file=file[[contract_id]], append=TRUE)
    # Write to file every 10 counts
    # if ((e_wrapper$da_ta$count_er %% 10) == 0) {
    #   for (contract_id in 1:(e_wrapper$da_ta$n_contracts)) {
    #     data.table::fwrite(e_wrapper$da_ta$bar_data[[e_wrapper$da_ta$contract_id]], file=file[[e_wrapper$da_ta$contract_id]])
    #   }  # end for
    # }  # end if

    # Write to console
    # cat(c(e_wrapper$da_ta$name_s[[contract_id]], paste(e_wrapper$da_ta$bar_data[[contract_id]][e_wrapper$da_ta$count_er, ], collapse=",")), "\n")
    cat(paste0("count_er=", count_er), paste0("vol=", round(e_wrapper$da_ta$vols[contract_id], 2)), paste0(colnames(e_wrapper$da_ta$bar_data[[contract_id]]), "=", e_wrapper$da_ta$bar_data[[contract_id]][count_er, ]), "\n")
    # cat("Number of rows of data for instrument ", contract_id, " is = ", NROW(e_wrapper$da_ta$bar_data[[e_wrapper$da_ta$contract_id]]), "\n")
    # cat(paste0("Open=", new_bar[4], "\tHigh=", new_bar[5], "\tLow=", new_bar[6], "\tClose=", new_bar[7], "\tVolume=", new_bar[8]), "\n")

    # Run the trading model
    if (!is.na(trade_params) && (count_er > e_wrapper$da_ta$warm_up))
      e_wrapper$model_fun(contract_id, trade_params, ib_connect)
    # Return values
    c(curMsg, msg)
  }  # end realtimeBars

  return(e_wrapper)
}  # end trade_wrapper




#' @details The function \code{create_ewrapper()} creates an eWrapper
#'   environment combined with handler functions.
#' @export
create_ewrapper <- function(debug=FALSE, errfile=stderr()) {
  da_ta <- new.env()
  # Create accessor functions
  ge_t <- function(x) get(x, da_ta)
  as_sign <- function(x, value) assign(x, value, da_ta)
  re_move <- function(x) remove(x, da_ta)
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
      symbols <- ge_t("symbols")
      e_tick_price(NULL, msg, timestamp, file, symbols, ...)
    }
    tickSize <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_tick_size(NULL, msg, timestamp, file, symbols, ...)
    }
    tickOptionComputation <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_tick_option(NULL, msg, timestamp, file, symbols, ...)
    }
    tickGeneric <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_tick_generic(NULL, msg, timestamp, file, symbols, ...)
    }
    tickString <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_tick_string(NULL, msg, timestamp, file, symbols, ...)
    }
    tickEFP <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_tick_EFP(NULL, msg, timestamp, file, symbols, ...)
    }
    orderStatus <- function(curMsg, msg, timestamp, file, ...) {
      e_order_status(curMsg, msg)
      c(curMsg, msg)
    }
    errorMessage <- function(curMsg, msg, timestamp, file, twsconn, ...) {
      if (msg[3] == "1100")
        twsconn$connected <- FALSE
      if (msg[3] %in% c("1101", "1102"))
        twsconn$connected <- TRUE
      cat("TWS Message:", msg, "\n")
    }
    openOrder <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    openOrderEnd <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    updateAccountValue <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    updatePortfolio <- function(curMsg, msg, timestamp, file, ...) {
      e_portfolio_value(curMsg, msg)
      c(curMsg, msg)
    }
    updateAccountTime <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    accountDownloadEnd <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    nextValidId <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    contractDetails <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    bondContractDetails <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    contractDetailsEnd <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    execDetails <- function(curMsg, msg, timestamp, file, ...) {
      e_execDetails(curMsg, msg, file, ...)
    }
    execDetailsEnd <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    updateMktDepth <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_update_mkt_depth(NULL, msg, timestamp, file, symbols, ...)
    }
    updateMktDepthL2 <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_update_mkt_depthL2(NULL, msg, timestamp, file, symbols, ...)
    }
    updateNewsBulletin <- function(curMsg, msg, timestamp, file, ...) {
      cat("newsMsgId: ", msg[2], "newsMsgType: ", msg[3],
          "newsMessage: ", msg[4], "origExch:", msg[5], "\n")
      c(curMsg, msg)
    }
    managedAccounts <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    receiveFA <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    historicalData <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    scannerParameters <- function(curMsg, msg, timestamp, file, ...) {
      cat(msg <- rawToChar(msg[-which(msg == as.raw(0))]))
      c(curMsg, msg)
    }
    scannerData <- function(curMsg, reqId, rank, contract,
                            distance, benchmark, projection, legsStr) {
      e_scannerData(curMsg, reqId, rank, contract, distance,
                    benchmark, projection, legsStr)
    }
    scannerDataEnd <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
      symbols <- ge_t("symbols")
      e_real_time_bars(curMsg, msg, symbols, file, ...)
    }
    currentTime <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    fundamentalData <- function(curMsg, msg, timestamp, file, ...) {
      e_fundamentalData(curMsg, msg)
    }
    deltaNeutralValidation <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
    tickSnapshotEnd <- function(curMsg, msg, timestamp, file, ...) {
      c(curMsg, msg)
    }
  }
  else {
    tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg, msg, timestamp, file, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n", file=file[[1]], append=TRUE, ...)
    }
    errorMessage <- function(curMsg, msg, timestamp, file, twsconn, ...) {
      cat(as.character(timestamp), curMsg, msg, "\n", file=file[[1]], append=TRUE, ...)
    }
  }
  e_wrapper <- list(da_ta=da_ta, ge_t=ge_t, as_sign=as_sign,
                 re_move=re_move, tickPrice=tickPrice, tickSize=tickSize,
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


