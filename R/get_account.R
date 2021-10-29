cancelAccountUpdates <- function(ib_connect, acctCode="1") {
  if(!is.twsConnection(ib_connect))
    stop("requires twsConnection object")
  .get_account(ib_connect, "0", acctCode)
}  # end cancelAccountUpdates


.get_account <- function(ib_connect, subscribe=TRUE, acctCode="1") {
  if (!is.twsConnection(ib_connect))
    stop("requires twsConnection object")

  sock_et <- ib_connect[[1]]
  VERSION <- "2"

  # send messages to TWS
  writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, sock_et)
  writeBin(VERSION, sock_et)
  writeBin(as.character(as.numeric(subscribe)), sock_et)
  writeBin(as.character(acctCode), sock_et)
}  # end .get_account


# Run callback loop without CALLBACK function
# eventWrapper is not used
get_account <- function(ib_connect,
                        subscribe=TRUE,
                        acctCode="1",
                        file_connects=NULL,
                        eventWrapper=eWrapper(),
                        CALLBACK=twsCALLBACK, ...) {
  if (!is.twsConnection(ib_connect))
    stop("requires twsConnection object")

  .get_account(ib_connect, subscribe, acctCode)

  on.exit(.get_account(ib_connect, "0", acctCode))

  verbose <- FALSE
  acct <- list()
  sock_et <- ib_connect[[1]]
  e_wrapper <- eWrapper(NULL)
  e_wrapper$assign.Data("data", structure(list(), class="eventAccountValue"))

  # Define callback function called by processMsg
  e_wrapper$updatePortfolio <- function (curMsg, msg, ...) {
    version <- as.numeric(msg[1])
    contract <- twsContract(conId=msg[2], symbol=msg[3],
                            sectype=msg[4], exch=msg[9], primary=msg[9], expiry=msg[5],
                            strike=msg[6], currency=msg[10], right=msg[7],
                            local=msg[11], multiplier=msg[8], combo_legs_desc="",
                            comboleg="", include_expired="")
    portfolioValue <- list()
    portfolioValue$position <- as.numeric(msg[12])
    portfolioValue$marketPrice <- as.numeric(msg[13])
    portfolioValue$marketValue <- as.numeric(msg[14])
    portfolioValue$averageCost <- as.numeric(msg[15])
    portfolioValue$unrealizedPNL <- as.numeric(msg[16])
    portfolioValue$realizedPNL <- as.numeric(msg[17])
    portfolioValue$accountName <- msg[18]
    p <- structure(list(contract=contract, portfolioValue=portfolioValue), class="eventPortfolioValue")
    p
  }  # end updatePortfolio

  # Define callback function called by processMsg
  e_wrapper$updateAccountValue <- function (curMsg, msg, ...) {
    data <- e_wrapper$get.Data("data")
    data[[msg[2]]] <- c(value=msg[3], currency=msg[4])
    e_wrapper$assign.Data("data", data)
  }  # end updateAccountValue


  #  acct_msgs <- with(.twsIncomingMSG,
  #                   c(ACCT_VALUE,PORTFOLIO_VALUE,ACCT_UPDATE_TIME))

  # Callback loop
  timestamp <- NULL
  while (TRUE) {
    socketSelect(list(sock_et), FALSE, NULL)
    curMsg <- readBin(sock_et, "character", 1)
    cat("curMsg: ", curMsg, "\n")
    #    if (!curMsg %in% acct_msgs) {
    #      if (curMsg == .twsIncomingMSG$ERR_MSG) {
    #        if (!errorHandler(sock_et, verbose, OK=c(165, 300,
    #            366, 2104, 2106, 2107))) {
    #            warning("error in get_account details")
    #              break
    #            }
    #        }
    #        else {
    #            processMsg(curMsg, sock_et, e_wrapper, timestamp, file_connects)
    #              if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END)
    #                break
    #        }
    #    }
    if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
      acct[[length(acct) + 1]] <- processMsg(curMsg, sock_et, e_wrapper, timestamp, file_connects)
    } else {
      processMsg(curMsg, sock_et, e_wrapper, timestamp, file_connects)
    }
    if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END)
      break
  }  # end while

  return(structure(list(e_wrapper$get.Data("data"), acct), class="AccountUpdate"))

  # CALLBACK(ib_connect, eventWrapper, NULL, file_connects)
}  # end get_account
