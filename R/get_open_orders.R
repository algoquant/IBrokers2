cancelAccountUpdates <- function(ib_connect, acctCode="1") {
  if(!is.twsConnection(ib_connect))
    stop("requires twsConnection object")
  .get_open_orders(ib_connect, "0", acctCode)
}  # end cancelAccountUpdates


.get_open_orders <- function(ib_connect, subscribe=TRUE) {
  if (!IBrokers2::is.twsConnection(ib_connect))
    stop("requires twsConnection object")

  sock_et <- ib_connect[[1]]
  VERSION <- "1"

  # send messages to TWS
  writeBin(c(IBrokers2::.twsOutgoingMSG$REQ_OPEN_ORDERS, VERSION), sock_et)
  writeBin(as.character(as.numeric(subscribe)), sock_et)
}  # end .get_open_orders


# Run callback loop without CALLBACK function
# eventWrapper is not used
get_open_orders <- function(ib_connect,
                            subscribe=TRUE) {

  if (!IBrokers2::is.twsConnection(ib_connect))
    stop("requires twsConnection object")

  .get_open_orders(ib_connect, subscribe=TRUE)

  # on.exit(.get_open_orders(ib_connect, subscribe=FALSE))

  verbose <- FALSE
  acct <- list()
  sock_et <- ib_connect[[1]]

  socketSelect(list(sock_et), FALSE, NULL)
  curMsg <- readBin(sock_et, "character", 1L)
  msg <- readBin(sock_et, "character", 84)
  cat("curMsg: ", curMsg, "\n")

  .get_open_orders(ib_connect, subscribe=FALSE)
  c(curMsg=curMsg, msg=msg)

}  # end get_open_orders
