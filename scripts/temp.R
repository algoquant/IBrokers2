library(IBrokers2)

con_tracts <- list(es=IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201812"))
limit_prices <- list(ES=c(buy_spread=1.75, sell_spread=1.75))

# Create eWrapper environment
e_wrapper <- IBrokers2::eWrapper()


data_dir <- "C:/Develop/data/ib_data"
file_names <- file.path(data_dir, paste0(names(con_tracts), "_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

# connect
ib_connect <- IBrokers2::twsConnect(port=7497)


# subscribe
ticker_id <- IBrokers2::.reqOpenOrders(ib_connect)
ticker_id <- IBrokers2::.reqRealTimeBars(conn=ib_connect, Contract=con_tracts, useRTH=FALSE)

sock_et <- ib_connect[[1]]

# Read the message
da_ta <- readBin(sock_et, "character", 1L)
# Process the message
foo <- processMsg(curMsg=da_ta, con=sock_et, eWrapper=e_wrapper, timestamp=NULL, file=file_connects, twsconn=ib_connect)




tryCatch(
  # Callback loop
  while(isConnected(ib_connect)) {
    if (!socketSelect(list(ib_connect), FALSE, 0.25)) next
    # Read the message
    da_ta <- readBin(ib_connect, "character", 1L)
    # Process the message
    processMsg(curMsg=da_ta, twsconn=ib_connect, eWrapper=e_wrapper, timestamp=NULL, file=file_connects, con=ib_connect)
  },  # end while
  # error handler
  error=function(e) { close(ib_connect); stop("IB connection error. Connection closed", call.=FALSE) }
)  # end tryCatch


# Close IB connection
IBrokers2::twsDisconnect(ib_connect)

