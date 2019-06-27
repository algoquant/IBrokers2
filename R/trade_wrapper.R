# n_contracts is the number of contracts in the data buffer
trade_wrapper <- function(ac_count,
                          con_tracts=NULL,
                          trade_params=NULL,
                          file_connects,
                          warm_up=100) { # warmup period
  # cat("Entering trade_wrapper", "\n")

  ## Create eWrapper environment
  e_wrapper <- create_ewrapper(NULL)
  e_wrapper$da_ta$ac_count <- ac_count
  e_wrapper$da_ta$warm_up <- warm_up
  # e_wrapper <- new.env()
  # Create eWrapper accessor functions
  # e_wrapper$get_ <- function(x) get(x, e_wrapper)
  # e_wrapper$assign_ <- function(x, value) assign(x, value, e_wrapper)
  # e_wrapper$remove_ <- function(x) remove(x, e_wrapper)
  # Initialize state variables in eWrapper environment
  # e_wrapper$as_sign("count_er", 0)

  ## Define contracts and variables for single instrument
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
    # Matrix of portfolio information
    e_wrapper$da_ta$portfolio <- matrix(numeric(3*n_contracts), ncol=3)
    colnames(e_wrapper$da_ta$portfolio) <- c("position", "pnl_realized", "pnl_unrealized")
    # Vector of volatilities, one for each contract
    e_wrapper$da_ta$vol <- numeric(n_contracts)
    # Vector of EWMAs, one for each contract
    e_wrapper$da_ta$ewma <- numeric(n_contracts)
    # Vector of open orders, one for each contract
    e_wrapper$da_ta$open_orders <- integer(n_contracts)
    # e_wrapper$da_ta$beta <- numeric(1)
  }  # end if

  ## Create data buffer for trade parameters
  if (is.null(trade_params))
    stop("trade_params argument is missing")
  else {
    if (!(n_contracts == NROW(trade_params)))
      stop("trade_params argument is missing some elements")
    e_wrapper$da_ta$trade_params <- trade_params
  }  # end if

  ## Create trade ids buffer, as a matrix of trade ids in the eWrapper environment
  # IB uses trade ids to keep track of trades
  e_wrapper$da_ta$trade_ids <- matrix(rep(NA_integer_, 2*n_contracts), ncol=2)
  colnames(e_wrapper$da_ta$trade_ids) <- c("buy_id", "sell_id")
  # names(e_wrapper$da_ta$trade_ids) <- e_wrapper$da_ta$name_s

  ## Define dimensions of data buffer for single instrument
  col_names <- c("Time", "Open", "High", "Low", "Close", "Volume", "WAP", "Count")
  # e_wrapper$as_sign("col_names", col_names)
  e_wrapper$da_ta$col_names <- col_names
  n_rows <- 8*60*12; n_cols <- NROW(col_names)
  e_wrapper$da_ta$n_rows <- n_rows
  e_wrapper$da_ta$n_cols <- n_cols
  e_wrapper$da_ta$col_index <- (3:(n_cols+2))

  ## Create data buffer bar_data, a list of matrices in the eWrapper environment
  e_wrapper$da_ta$bar_data <- rep(list(matrix(rep(NA_real_, n_rows*n_cols), ncol=n_cols)), n_contracts)
  names(e_wrapper$da_ta$bar_data) <- name_s
  for (it in 1:NROW(name_s)) {
    col_n <- paste(name_s[it], col_names, sep=".")
    colnames(e_wrapper$da_ta$bar_data[[it]]) <- col_n
    # Write headers to data files
    cat(paste(col_n, collapse=","), "\n", file=file_connects[[it]], append=TRUE)
  }  # end for

  ## Create limit_prices, a list of matrices with past limit prices
  # e_wrapper$da_ta$limit_prices <- lapply(trade_params, function(trade_param) {
  #   if (!is.na(trade_param) && (trade_param["lagg"] > 0)) {
  #     nn <- (trade_param["lagg"]+1)
  #     structure(matrix(numeric(2*nn), nc=2), dimnames=list(rows=1:nn, columns=c("buy_limit", "sell_limit")))
  #   }  # end if
  #   else
  #     0
  # })  # end lapply


  ## Define function realtimeBars inside the eWrapper environment
  # realtimeBars() processes a new bar of data and runs the model_function()
  # realtimeBars() is called by processMsg() in a callback loop inside twsCALLBACK()
  e_wrapper$realtimeBars <- function(curMsg, msg, timestamp, file, ib_connect, ...) {
    # cat("realtimeBars count_er=", e_wrapper$da_ta$count_er, "\n")
    # browser()
    # Unwrap new bar of data in msg passed from processMsg()
    bar_new <- as.numeric(msg)
    names(bar_new) <- c("some_id", "contract_id", e_wrapper$da_ta$col_names)
    # cat("realtimeBars col_names: ", col_names, "\n")
    # cat("realtimeBars n_cols: ", n_cols, "\n")
    # col_index <- (3:(e_wrapper$da_ta$n_cols+2))
    # names(bar_new)[e_wrapper$da_ta$col_index] <- e_wrapper$da_ta$col_names
    contract_id <- bar_new[2]
    trade_params <- e_wrapper$da_ta$trade_params[[contract_id]]
    # If trade_params is not NULL then trade that instrument
    trade_it <- !is.null(trade_params)
    if (trade_it) {
      # model_function <- match.fun("make_markets")
      model_function <- trade_params[[1]]
      trade_params <- trade_params[[2]]
    }  # end if
    # e_wrapper$as_sign("count_er", e_wrapper$ge_t("count_er")+1)
    # Advancce the bar count_er
    count_er <- e_wrapper$da_ta$count_er[contract_id] + 1
    e_wrapper$da_ta$count_er[contract_id] <<- count_er
    # assign("count_er", count_er, e_wrapper$da_ta)
    # cat("realtimeBars bar_new: ", bar_new, "\n")
    # cat("realtimeBars: ", e_wrapper$ge_t("count_er"), "\n")
    # Copy new bar of data into buffer
    # browser()
    e_wrapper$da_ta$bar_data[[contract_id]][count_er, ] <<- bar_new[e_wrapper$da_ta$col_index]
    # cat("realtimeBars: ", e_wrapper$da_ta$name_s[contract_id], " vol: ", e_wrapper$da_ta$vol[contract_id], "\n")

    ## Update EWMA and volatility
    lamb_da <- trade_params["lamb_da"]
    if (trade_it && (count_er > 3)) {
      e_wrapper$da_ta$vol[contract_id] <<- lamb_da*(bar_new["High"]-bar_new["Low"]) + (1-lamb_da)*e_wrapper$da_ta$vol[contract_id]
      e_wrapper$da_ta$ewma[contract_id] <<- lamb_da*bar_new["WAP"] + (1-lamb_da)*e_wrapper$da_ta$ewma[contract_id]
    } else {
      e_wrapper$da_ta$vol[contract_id] <<- (bar_new["High"]-bar_new["Low"])
      e_wrapper$da_ta$ewma[contract_id] <<- bar_new["WAP"]
    }  # end if

    ## Download from IB portfolio information: net positions, pnls
    if (trade_it && IBrokers2::is.twsConnection(ib_connect)) {
        ib_account <- IBrokers2::reqAccountUpdates(conn=ib_connect, acctCode=e_wrapper$da_ta$ac_count)
        if (!is.null(ib_account)) {
          port_folio <- unlist(ib_account[[2]])
          e_wrapper$da_ta$portfolio[contract_id, "position"] <- port_folio["portfolioValue.position"]
          e_wrapper$da_ta$portfolio[contract_id, "pnl_realized"] <- port_folio["portfolioValue.realizedPNL"]
          e_wrapper$da_ta$portfolio[contract_id, "pnl_unrealized"] <- port_folio["portfolioValue.unrealizedPNL"]
        }  # end if
        cat("Portfolio information from IB, position: ", e_wrapper$da_ta$portfolio[contract_id, "position"],
            "\t realized pnl: ", e_wrapper$da_ta$portfolio[contract_id, "pnl_realized"],
            "\t unrealized pnl: ", e_wrapper$da_ta$portfolio[contract_id, "pnl_unrealized"], "\n")
      }  # end if

    ## Write bar data to file
    # file_name <- file[[contract_id]]
    cat(paste(e_wrapper$da_ta$bar_data[[contract_id]][count_er, ], collapse=","), "\n", file=file[[contract_id]], append=TRUE)

    # Write to file every 10 counts
    # if ((e_wrapper$da_ta$count_er %% 10) == 0) {
    #   for (contract_id in 1:(e_wrapper$da_ta$n_contracts)) {
    #     data.table::fwrite(e_wrapper$da_ta$bar_data[[e_wrapper$da_ta$contract_id]], file=file[[e_wrapper$da_ta$contract_id]])
    #   }  # end for
    # }  # end if

    ## Write to console
    # cat(paste0("realtimeBars count_er=", count_er), paste0("ewma=", round(e_wrapper$da_ta$ewma[contract_id], 2)), paste0(colnames(e_wrapper$da_ta$bar_data[[contract_id]]), "=", e_wrapper$da_ta$bar_data[[contract_id]][count_er, ]), "\n")
    # cat(c(e_wrapper$da_ta$name_s[[contract_id]], paste(e_wrapper$da_ta$bar_data[[contract_id]][e_wrapper$da_ta$count_er, ], collapse=",")), "\n")
    # cat("Number of rows of data for instrument ", contract_id, " is = ", NROW(e_wrapper$da_ta$bar_data[[e_wrapper$da_ta$contract_id]]), "\n")
    # cat(paste0("Open=", bar_new[4], "\tHigh=", bar_new[5], "\tLow=", bar_new[6], "\tClose=", bar_new[7], "\tVolume=", bar_new[8]), "\n")

    ## Run the trading model for the trading instrument
    if (trade_it && (count_er > e_wrapper$da_ta$warm_up))
      # make_markets(contract_id, trade_params, ib_connect)
      do.call(model_function, list(contract_id, trade_params, ib_connect))
    # Return values
    c(curMsg, msg)
  }  # end realtimeBars


  ## Define trading model functions inside the eWrapper environment
  # The trading model functions are called by realtimeBars()

  ### The function make_markets() performs a market making strategy by
  # executing limit orders on a single instrument, with position limits.
  make_markets <- function(contract_id, trade_params, ib_connect) {
    # if (!IBrokers2::isConnected(ib_connect)) {ib_connect <- IBrokers2::twsConnect(port=7497) ; cat("reconnected")}
    # cat("make_markets count_er: ", e_wrapper$da_ta$count_er, "\n")
    # browser()

    ## Extract bars of prices from da_ta$bar_data
    count_er <- e_wrapper$da_ta$count_er[contract_id]
    # it <- (count_er - trade_params["lagg"])
    bar_new <- e_wrapper$da_ta$bar_data[[contract_id]][count_er, ]
    bar_lag <- e_wrapper$da_ta$bar_data[[contract_id]][count_er - trade_params["lagg"], ]
    # browser()

    ## Write to console
    cat(paste0("make_markets count_er=", count_er), "\n", "bar data: \t", paste0(names(bar_new), "=", bar_new), "\n")
    cat("technical data: \t", paste0("vol=", round(e_wrapper$da_ta$vol[contract_id], 2)), paste0("ewma=", round(e_wrapper$da_ta$ewma[contract_id], 2)), "\n")

    ## Experimental code - turned off for now
    # Calculate the trade price bia_s by comparing the lagged WAP with EWMA
    # scale the bia_s by the vol level - default is zero
    # bia_s <- 0.25*trunc(e_wrapper$da_ta$fac_tor*e_wrapper$da_ta$vol[contract_id])
    # bia_s <- (if (bar_lag[7] > e_wrapper$da_ta$ewma[contract_id]) 0.25 else -0.25)
    ## End experimental code - turned off for now
    bia_s <- 0
    # cat("make_markets limit bia_s=", bia_s, "\n")
    # cat(paste0("make_markets bia_s=", bia_s), paste0("ewma=", round(e_wrapper$da_ta$ewma[contract_id], 2)), paste0("lag_", e_wrapper$da_ta$col_names, "=", bar_lag), "\n")

    ## Calculate the trade limit prices
    # Limit prices are the low and high prices of the lagged bar, plus the spreads
    buy_limit <- (bar_lag[4] - trade_params["buy_spread"] + bia_s)
    sell_limit <- (bar_lag[3] + trade_params["sell_spread"] + bia_s)
    # buy_limit should be no greater than close price
    # clo_se <- e_wrapper$da_ta$bar_data[[contract_id]][count_er, 5]
    clo_se <- bar_new[5]
    buy_limit <- min(clo_se, buy_limit)
    # sell_limit should be no less than close price
    sell_limit <- max(clo_se, sell_limit)

    ## Cancel previous limit orders
    if (IBrokers2::is.twsConnection(ib_connect)) {
      buy_id <- e_wrapper$da_ta$trade_ids[contract_id, "buy_id"]
      if (!is.na(buy_id))
        IBrokers2::cancelOrder(ib_connect, buy_id)
      sell_id <- e_wrapper$da_ta$trade_ids[contract_id, "sell_id"]
      if (!is.na(sell_id))
        IBrokers2::cancelOrder(ib_connect, sell_id)
    }  # end if

    ## Place new limit orders
    # cat("make_markets trade_params: ", trade_params, "\n")
    if (IBrokers2::is.twsConnection(ib_connect)) {
      # Live trading mode
      posi_tion <- e_wrapper$da_ta$portfolio[contract_id, "position"]
      # Place buy limit order only if position doesn't exceed limit
      if (posi_tion < trade_params["pos_limit"]) {
        buy_id <- IBrokers2::reqIds(ib_connect)
        buy_order <- IBrokers2::twsOrder(buy_id, orderType="LMT",
                                         lmtPrice=buy_limit, action="BUY",
                                         totalQuantity=trade_params["siz_e"])
        IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], buy_order)
        e_wrapper$da_ta$trade_ids[contract_id, "buy_id"] <<- buy_id
      }  # end if

      # Place sell limit order only if position doesn't exceed limit
      if (posi_tion > -trade_params["pos_limit"]) {
        sell_id <- IBrokers2::reqIds(ib_connect)
        sell_order <- IBrokers2::twsOrder(sell_id, orderType="LMT",
                                          lmtPrice=sell_limit, action="SELL",
                                          totalQuantity=trade_params["siz_e"])
        IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], sell_order)
        e_wrapper$da_ta$trade_ids[contract_id, "sell_id"] <<- sell_id
      }  # end if

    } else {
      # Backtest mode: write to file
      cat("Buy limit:,", buy_limit, ",Sell limit:,", sell_limit, "\n", file=ib_connect, append=TRUE)
      browser()
    }  # end if
    # cat("Buy limit order at: ", buy_limit, "\tSell limit order at: ", sell_limit, "\n")

    invisible(list(buy_limit=buy_limit, sell_limit=sell_limit))
  }  # end make_markets


  ### The function crossover_strat() performs an EWMA crossover strategy by
  # executing market orders on a single instrument
  crossover_strat <- function(contract_id, trade_params, ib_connect) {
    # if (!IBrokers2::isConnected(ib_connect)) {ib_connect <- IBrokers2::twsConnect(port=7497) ; cat("reconnected")}
    # cat("crossover_strat count_er: ", e_wrapper$da_ta$count_er, "\n")
    # browser()

    ## Extract bars of prices from da_ta$bar_data
    count_er <- e_wrapper$da_ta$count_er[contract_id]
    # it <- (count_er - trade_params["lagg"])
    bar_new <- e_wrapper$da_ta$bar_data[[contract_id]][count_er, ]
    # bar_lag <- e_wrapper$da_ta$bar_data[[contract_id]][count_er - trade_params["lagg"], ]
    # browser()

    ## Write to console
    ew_ma <- e_wrapper$da_ta$ewma[contract_id]
    w_ap <- bar_new[7]
    cat(paste0("crossover_strat count_er=", count_er), "\n", "bar data: \t", paste0(names(bar_new), "=", bar_new), "\n")
    cat("technical data: \t", paste0("WAP=", round(w_ap, 2)), paste0("EWMA=", round(ew_ma, 2)), "\n")

    ## Place market orders
    # cat("crossover_strat trade_params: ", trade_params, "\n")
    if (IBrokers2::is.twsConnection(ib_connect)) {
      # Live trading mode
      posi_tion <- e_wrapper$da_ta$portfolio[contract_id, "position"]
      is_contrarian <- trade_params["is_contrarian"]
      if (is_contrarian) {
        if ((w_ap > ew_ma) & (posi_tion > 0)) {
          # Place buy market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="SELL", totalQuantity=trade_params["siz_e"])
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
        } else if ((w_ap < ew_ma) & (posi_tion < 0)) {
          # Place sell market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=trade_params["siz_e"])
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
        }  # end if
      } else {
        if ((w_ap > ew_ma) & (posi_tion < 0)) {
          # Place buy market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=trade_params["siz_e"])
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
        } else if ((w_ap < ew_ma) & (posi_tion > 0)) {
          # Place sell market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="SELL", totalQuantity=trade_params["siz_e"])
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
        }  # end if
      }  # end if is_contrarian

    } else {
      # Backtest mode: write to file
      cat("Buy limit:,", buy_limit, ",Sell limit:,", sell_limit, "\n", file=ib_connect, append=TRUE)
      browser()
    }  # end if
    # cat("Buy limit order at: ", buy_limit, "\tSell limit order at: ", sell_limit, "\n")

    invisible(list(WAP=w_ap, EWMA=ew_ma))
  }  # end crossover_strat



  ### The function pairs_strat() performs a pairs trading strategy
  # by executing market orders for a pair of instruments
  pairs_strat <- function(contract_id, trade_params, ib_connect) {
    # if (!IBrokers2::isConnected(ib_connect)) {ib_connect <- IBrokers2::twsConnect(port=7497) ; cat("reconnected")}
    # cat("pairs_strat count_er: ", e_wrapper$da_ta$count_er, "\n")
    # browser()

    look_back <- trade_params["look_back"]
    thresh_old <- trade_params["thresh_old"]
    posi_tion <- e_wrapper$da_ta$portfolio[contract_id, "position"]
    siz_e <- trade_params["siz_e"]
    is_contrarian <- trade_params["is_contrarian"]
    ## Extract bars of prices from da_ta$bar_data
    count_er <- e_wrapper$da_ta$count_er[contract_id]
    # it <- (count_er - trade_params["lagg"])
    bar_new <- e_wrapper$da_ta$bar_data[[contract_id]][((count_er-look_back+1):count_er), ]
    bar_ref <- e_wrapper$da_ta$bar_data[[2]][((count_er-look_back+1):count_er), ]

    ## Perform regression in C++
    de_sign <- cbind(bar_new[, 7], bar_ref[, 7])
    de_sign <- na.omit(de_sign)
    colnames(de_sign) <- e_wrapper$da_ta$name_s
    l_m <- HighFreq::calc_lm(de_sign[, 1], de_sign[, 2, drop=FALSE])
    co_eff <- l_m$coefficients[2, 1]
    z_score <- l_m$z_score

    ## Write to console
    ew_ma <- e_wrapper$da_ta$ewma[contract_id]
    w_ap <- bar_new[NROW(bar_new), 7]
    # cat(paste0("pairs_strat count_er=", count_er), "\n", "bar data: \t", last(bar_new), "\n")
    # cat(paste0("pairs_strat count_er=", count_er), "\n", "bar data: \t", last(bar_ref), "\n")
    cat("technical data: \t", paste0("coeff=", round(co_eff, 2)), paste0("z-score=", round(z_score, 2)), "\n")
    # cat("technical data: \t", paste0("WAP=", round(w_ap, 2)), paste0("EWMA=", round(ew_ma, 2)), "\n")


    ## Place market orders
    # cat("pairs_strat trade_params: ", trade_params, "\n")
    if (IBrokers2::is.twsConnection(ib_connect)) {
      # Live trading mode
      # browser()
      n_shares <- round(co_eff*siz_e)
      if (is_contrarian) {
        if (!is.na(z_score) && (z_score > thresh_old) && (posi_tion > 0)) {
          # Place buy market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="SELL", totalQuantity=siz_e)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=n_shares)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[2]], ib_order)
        } else if (!is.na(z_score) && (z_score < -thresh_old) && (posi_tion < 0)) {
          # Place sell market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=siz_e)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="SELL", totalQuantity=n_shares)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[2]], ib_order)
        }  # end if
      } else {
        if (!is.na(z_score) && (z_score > thresh_old) && (posi_tion > 0)) {
          # Place buy market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=siz_e)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="SELL", totalQuantity=n_shares)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[2]], ib_order)
        } else if (!is.na(z_score) && (z_score < -thresh_old) && (posi_tion < 0)) {
          # Place sell market order
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="SELL", totalQuantity=siz_e)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[contract_id]], ib_order)
          order_id <- IBrokers2::reqIds(ib_connect)
          ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=n_shares)
          IBrokers2::placeOrder(ib_connect, e_wrapper$da_ta$con_tracts[[2]], ib_order)
        }  # end if
      }  # end if is_contrarian

    } else {
      # Backtest mode: write to file
      cat("Buy limit:,", buy_limit, ",Sell limit:,", sell_limit, "\n", file=ib_connect, append=TRUE)
      browser()
    }  # end if
    # cat("Buy limit order at: ", buy_limit, "\tSell limit order at: ", sell_limit, "\n")

    invisible(list(co_eff=co_eff, z_score=z_score))
  }  # end pairs_strat


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


