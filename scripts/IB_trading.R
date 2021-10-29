####################################
### Script for trading through Interactive Brokers API
### using package IBrokers2.

### WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
### THESE SCRIPTS ARE FOR PAPER TRADING ONLY.
### DO NOT USE THEM FOR LIVE TRADING WITH REAL CAPITAL!

### This is only a proof of concept for testing the API code.
### This is NOT a realistic trading algorithm.
### It will lose money in actual trading.
### Do NOT use this code with real money.
### Use it ONLY for paper trading.


### Load packages

# Install package *rutils* from github:
devtools::install_github(repo="algoquant/rutils")
library(rutils)

# Install package *IBrokers2* from github:
devtools::install_github(repo="algoquant/IBrokers2")
library(IBrokers2)

# Load the trading function written as an eWrapper:
# source("C:/Develop/R/IBrokers2/R/trade_wrapper.R")


### Scripts for running a simple trading strategy in a callback loop:

# The script will also save bar data into files

# The simple market-making strategy is defined as follows:
#  Place limit buy order at previous bar Low price minus buy_spread,
#  Place limit sell order at previous bar High price plus sell_spread.
#
# The trading strategy functions are defined inside the function
# realtimeBars(), which is part of the trade wrapper.
# The user can customize the strategy or create a new strategy by
# modifying the trading function code in the function realtimeBars()
# in the file trade_wrapper.R


## Define parameters for strategies

# Define instrument for trading one ES futures contract
con_tracts <- list(ES=IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201912"))

# Define parameters of market making strategy
trade_params <- list(ES=list(model_function="make_markets",
                             model_params=c(buy_spread=0.25, sell_spread=0.25, siz_e=1, pos_limit=10, lagg=2, lamb_da=0.05)))

# Define parameters of EWMA crossover strategy
trade_params <- list(ES=list(model_function="crossover_strat",
                             model_params=c(is_contrarian=TRUE, siz_e=2, lamb_da=0.2)))


# Define instruments for trading two stocks: GOOG and AAPL
# con_tract <- IBrokers::twsEquity("AAPL", primary="SMART")
sym_bols <- c("GOOG", "AAPL")
con_tracts <- lapply(sym_bols, IBrokers2::twsEquity, primary="SMART")
names(con_tracts) <- sym_bols

# Define parameters for pairs trading two stocks: GOOG and AAPL
# The traded instrument is second in the list so that the model
# is run on contemporaneous data, after the bars for both instruments arrive.
trade_params <- list(AAPL=NULL,
                     GOOG=list(model_function="pairs_strat",
                               model_params=c(is_contrarian=TRUE, siz_e=100, look_back=5, thresh_old=1.0, lamb_da=0.2)))


## Open the files for storing the bar data
data_dir <- "C:/Develop/data/ib_data"
file_names <- file.path(data_dir, paste0(names(con_tracts), "_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

## Open the IB connection to TWS
ac_count <- "DU1851021"
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


# extra

### Scripts for running a simple trading strategy in a callback loop:

# The script will also save bar data into files

# The simple market-making strategy is defined as follows:
#  Place limit buy order at previous bar Low price minus buy_spread,
#  Place limit sell order at previous bar High price plus sell_spread.
#
# The strategy is defined inside the function model_fun() which
# is part of the eWrapper defined by trade_wrapper().
# The user can customize this strategy by modifying the trading
# code in the function model_fun().

# Define named lists for trading one contract
con_tracts <- list(ES=IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201912"))
trade_params <- list(ES=c(buy_spread=0.75, sell_spread=0.75, siz_e=1, lagg=2, lamb_da=0.05))

# Open the files for storing the bar data
data_dir <- "C:/Develop/data/ib_data"
file_names <- file.path(data_dir, paste0(names(con_tracts), "_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

# Open the IB connection to TWS
ac_count <- "DU1851021"
ib_connect <- IBrokers2::twsConnect(port=7497)

# Run the trading model (strategy):
IBrokers2::trade_realtime(ib_connect=ib_connect,
                          Contract=con_tracts,
                          useRTH=FALSE,
                          back_test=FALSE,
                          eventWrapper=IBrokers2::trade_wrapper(ac_count=ac_count,
                                                                con_tracts=con_tracts,
                                                                trade_params=trade_params,
                                                                file_connects=file_connects,
                                                                warm_up=10),
                          CALLBACK=IBrokers2::call_back,
                          file=file_connects)

# Stop the trading loop by hitting the red STOP button in RStudio

# Close IB connection
IBrokers2::twsDisconnect(ib_connect)

# Close data files
for (file_connect in file_connects) close(file_connect)




