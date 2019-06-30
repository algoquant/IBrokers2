####################################
### R scripts for the presentation by Jerzy Pawlowski, titled:
# Package iBrokers2 for Real-time Trading With Interactive Brokers
# Given on June 27th, 2019, at the
# Stevens Conference on High-Frequency Finance and Analytics.

# These are R scripts for PAPER TRADING through Interactive
# Brokers API using package IBrokers2.

### WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
### THESE SCRIPTS ARE FOR PAPER TRADING ONLY.
### DO NOT USE THEM FOR LIVE TRADING WITH REAL CAPITAL!


############ DISCLAIMER ############

# The package IBrokers2 allows users to test their own
# trading strategies in paper trading, without having
# to program the IB API directly.
# It can be an element of a toolkit for developing
# trading strategies, and testing them in paper trading.
# The package IBrokers2 is NOT a software product for
# inexperienced users, and it requires strong knowledge
# of R and the IB API.
# The package IBrokers2 does NOT provide realistic
# trading strategies.
# The package IBrokers2 comes with no warranty, and it
# is not affiliated or endorsed by Interactive Brokers.


############ INSTRUCTIONS ############

# Please install the Interactive Brokers Trader Workstation
# (TWS) on your computer.
# Please install RStudio, and run the following scripts from
# RStudio.
# The following scripts require the user to be logged into
# the TWS on the same computer as the one running the scripts.
# The scripts will place trade orders to IB, and they will
# also save the bar data into files on your computer.

## Load packages

# Install package *rutils* from github:
devtools::install_github(repo="algoquant/rutils")
library(rutils)

# Install package *IBrokers2* from github:
devtools::install_github(repo="algoquant/IBrokers2")
library(IBrokers2)

# Enter your account number
ac_count <- "DU1851021"


### Define parameters for a naive market-making strategy

# Define instrument for trading one ES futures contract
con_tracts <- list(ES=IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201909"))
# Define model parameters for market making strategy
trade_params <- list(ES=list(model_function="make_markets",
                             model_params=c(buy_spread=0.25, sell_spread=0.25, siz_e=1, pos_limit=10, lagg=2, lamb_da=0.05)))

# Open the files for storing the bar data
data_dir <- "C:/Develop/data/ib_data"
file_names <- file.path(data_dir, paste0(names(con_tracts), "_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

# Open the IB connection to TWS
ib_connect <- IBrokers2::twsConnect(port=7497)

# Run the trading model (strategy):
IBrokers2::trade_realtime(ib_connect=ib_connect,
                          Contract=con_tracts,
                          tickerId=seq_along(con_tracts),
                          useRTH=FALSE,
                          back_test=FALSE,
                          eventWrapper=IBrokers2::trade_wrapper(ac_count=ac_count,
                                                                con_tracts=con_tracts,
                                                                trade_params=trade_params,
                                                                file_connects=file_connects,
                                                                warm_up=6),
                          CALLBACK=IBrokers2::call_back,
                          file=file_connects)

# Stop the trading loop by hitting the red STOP button in RStudio

# Close IB connection
IBrokers2::twsDisconnect(ib_connect)

# Close data files
for (file_connect in file_connects) close(file_connect)



### Define parameters for EWMA crossover strategy

# Define model parameters for EWMA crossover strategy
trade_params <- list(ES=list(model_function="crossover_strat",
                             model_params=c(is_contrarian=TRUE, siz_e=2, lamb_da=0.2)))

# Open the files for storing the bar data
file_names <- file.path(data_dir, paste0(names(con_tracts), "_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

# Open the IB connection to TWS
ib_connect <- IBrokers2::twsConnect(port=7497)

# Run the trading model (strategy):
IBrokers2::trade_realtime(ib_connect=ib_connect,
                          Contract=con_tracts,
                          tickerId=seq_along(con_tracts),
                          useRTH=FALSE,
                          back_test=FALSE,
                          eventWrapper=IBrokers2::trade_wrapper(ac_count=ac_count,
                                                                con_tracts=con_tracts,
                                                                trade_params=trade_params,
                                                                file_connects=file_connects,
                                                                warm_up=6),
                          CALLBACK=IBrokers2::call_back,
                          file=file_connects)

# Stop the trading loop by hitting the red STOP button in RStudio

# Close IB connection
IBrokers2::twsDisconnect(ib_connect)

# Close data files
for (file_connect in file_connects) close(file_connect)



### Define parameters for a pairs trading strategy

# Define instruments for trading two stocks: GOOG and AAPL
sym_bols <- c("GOOG", "AAPL")
con_tracts <- lapply(sym_bols, IBrokers2::twsEquity, primary="SMART")
names(con_tracts) <- sym_bols

# Define model parameters for pairs trading two stocks: GOOG and AAPL
# The traded instrument is GOOG versus AAPL
trade_params <- list(AAPL=NULL,
                     GOOG=list(model_function="pairs_strat",
                               model_params=c(is_contrarian=TRUE, siz_e=100, look_back=5, thresh_old=1.0, lamb_da=0.2)))

# Open the files for storing the bar data
file_names <- file.path(data_dir, paste0(names(con_tracts), "_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

# Open the IB connection to TWS
ib_connect <- IBrokers2::twsConnect(port=7497)

# Run the trading model (strategy):
IBrokers2::trade_realtime(ib_connect=ib_connect,
                          Contract=con_tracts,
                          tickerId=seq_along(con_tracts),
                          useRTH=FALSE,
                          back_test=FALSE,
                          eventWrapper=IBrokers2::trade_wrapper(ac_count=ac_count,
                                                                con_tracts=con_tracts,
                                                                trade_params=trade_params,
                                                                file_connects=file_connects,
                                                                warm_up=6),
                          CALLBACK=IBrokers2::call_back,
                          file=file_connects)

# Stop the trading loop by hitting the red STOP button in RStudio

# Close IB connection
IBrokers2::twsDisconnect(ib_connect)

# Close data files
for (file_connect in file_connects) close(file_connect)


### Close (trade out of) any unwanted positions using market orders

# Open the IB connection to TWS
ib_connect <- IBrokers2::twsConnect(port=7497)

# Execute buy market order (example)
order_id <- IBrokers2::reqIds(ib_connect)
ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=10)
IBrokers2::placeOrder(ib_connect, con_tracts[[1]], ib_order)

# Execute sell market order (example)
order_id <- IBrokers2::reqIds(ib_connect)
ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="SELL", totalQuantity=10)
IBrokers2::placeOrder(ib_connect, con_tracts[[2]], ib_order)

# Close IB connection
IBrokers2::twsDisconnect(ib_connect)

