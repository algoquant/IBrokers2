####################################
### Scripts for trading through Interactive Brokers
### using package IBrokers.


library(HighFreq)


####################################
### Scripts for account information

# library(IButils)
# Install package *IBrokers2* from github:
# devtools::install_github(repo="algoquant/IBrokers2")
library(IBrokers2)

# IButils query
# IButils::flex_web_service(file = "C:/Develop/R/IBrokers/my_report.csv",
#                  token = "12345678901234567890",
#                  query = 123)

# Open the IB connection
ib_connect <- IBrokers2::twsConnect(port=7497)
# Download account information from IB
ib_account <- IBrokers::reqAccountUpdates(conn=ib_connect, acctCode="DI1207807")
# Extract account balances
balance_s <- ib_account[[1]]
balance_s$AvailableFunds
# Extract contract names, net positions, and profits and losses
IBrokers::twsPortfolioValue(ib_account)

### Not necessary:
# Open the files for storing the account information
data_dir <- "C:/Develop/data/ib_data"
file_names <- file.path(data_dir, "acct_info.txt")
file_connects <- file(file_names, open="w")
# file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

foo <- IBrokers2::get_account(ib_connect=ib_connect, acctCode="DI1207807", file_connects=file_connects)

ib_account <- IBrokers::.reqAccountUpdates(conn=ib_connect, subscribe=TRUE, acctCode="DI1207807")

# Doesn't work
# foo <- IBrokers2::reqExecutions(twsconn=ib_connect, ExecutionFilter=twsExecutionFilter)
# foo <- IBrokers2::reqOpenOrders(ib_connect)

# Close IB connection
IBrokers2::twsDisconnect(ib_connect)



####################################
### Scripts for running a simple trading strategy in a callback loop:

# Load the trading function written as an eWrapper:
# source("C:/Develop/R/IBrokers2/R/trade_wrapper.R")


# Define named lists for trading one contract
con_tracts <- list(ES=IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201909"))
con_tracts <- list(GBP=IBrokers2::twsFuture(symbol="GBP", exch="GLOBEX", expiry="201909"))
trade_params <- list(ES=c(buy_spread=0.75, sell_spread=0.75, siz_e=1, lagg=2, lamb_da=0.05))

# Define named lists for trading one contract and saving the others
con_tracts <- list(ES=IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201909"),
                   QM=IBrokers2::twsFuture(symbol="QM", exch="NYMEX", expiry="201909"),
                   # GBP=IBrokers2::twsCurrency("GBP", currency="USD"),
                   ZN=IBrokers2::twsFuture(symbol="ZN", exch="ECBOT", expiry="201909"))
trade_params <- list(ES=c(buy_spread=0.75, sell_spread=0.75, siz_e=1, lagg=2, lamb_da=0.05),
                     QM=NULL, ZN=NULL)
# trade_params <- list(ES=NULL, QM=NULL, GBP=c(buy_spread=0.001, sell_spread=0.001, siz_e=5e4, lagg=0, lamb_da=0.05), ZN=NULL)


# The simple market-making strategy is defined as follows:
#  Place limit buy order at previous bar Low price minus buy_spread,
#  Place limit sell order at previous bar High price plus sell_spread.
#
# The strategy is defined inside the function model_fun() which
# is part of the eWrapper defined by trade_wrapper().
# The user can customize this strategy by modifying the trading
# code in the function model_fun().

# Define trading model function

# Open the files for storing the bar data
data_dir <- "C:/Develop/data/ib_data"
file_names <- file.path(data_dir, paste0(names(con_tracts), "_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- lapply(file_names, function(file_name) file(file_name, open="w"))

# Open the IB connection to TWS
ib_connect <- IBrokers2::twsConnect(port=7497)
# Open the IB connection to output file
ib_connect <- file(file.path(data_dir, paste0("output_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv")),
                   open="w")


# Run the trading model (strategy):
IBrokers2::trade_realtime(ib_connect=ib_connect,
                          Contract=con_tracts,
                          useRTH=FALSE,
                          back_test=FALSE,
                          eventWrapper=IBrokers2::trade_wrapper(ac_count="DI1207807",
                                                                con_tracts=con_tracts,
                                                                trade_params=trade_params,
                                                                file_connects=file_connects,
                                                                warm_up=10),
                          CALLBACK=IBrokers2::call_back,
                          # CALLBACK=IBrokers2::twsCALLBACK,
                          file=file_connects)

# Execute buy market order
order_id <- IBrokers2::reqIds(ib_connect)
ib_order <- IBrokers2::twsOrder(order_id, orderType="MKT", action="BUY", totalQuantity=10)
IBrokers2::placeOrder(ib_connect, con_tracts[[1]], ib_order)

# Execute sell limit order
order_id <- IBrokers2::reqIds(ib_connect)
ib_order <- IBrokers2::twsOrder(order_id, orderType="LMT", lmtPrice="1.285", action="SELL", totalQuantity=5e4)
IBrokers2::placeOrder(ib_connect, con_tracts[["GBP"]], ib_order)


# Close IB connection
IBrokers2::twsDisconnect(ib_connect)

# Close data files
for (file_connect in file_connects) close(file_connect)


# IBrokers2::reqOpenOrders(ib_connect)


# Read the data from file
file_name <- "C:/Develop/data/ib_data/ES_ohlc_live11_02_2018_11_53.csv"
price_s <- data.table::setDF(data.table::fread(file_name, sep=","))
price_s <- xts::xts(price_s[, 2:6], as.POSIXct(price_s[, 1], origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="OHLC prices") %>% dyCandlestick()


####################################
### Download market data for two contracts simultaneously
### Interactive Brokers using package IBrokers.


con_tracts <- list(es=IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201909"),
                  tsy=IBrokers2::twsFuture(symbol="ZN",exch="ECBOT", expiry="201909"))

# Open the file for storing the bar data
data_dir <- "C:/Develop/data/ib_data"
file_name <- file.path(data_dir, paste0("ES_ohlc_live_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connects <- file(file_name, open="w")
# Open the IB connection
ib_connect <- IBrokers2::twsConnect(port=7497)

IBrokers2::reqRealTimeBars(conn=ib_connect,
                           Contract=con_tracts,
                           barSize="1", useRTH=FALSE,
                           eventWrapper=eWrapper.RealTimeBars.CSV(NROW(con_tracts)),
                           file=file_connects)

# Close IB connection
IBrokers2::twsDisconnect(ib_connect)
close(file_connects)

library(data.table)
price_s <- data.table::fread(file_name)
price_s <- xts::xts(price_s[, paste0("V", 2:6)],
                    as.POSIXct.numeric(as.numeric(price_s[, V1]), tz="America/New_York", origin="1970-01-01"))
colnames(price_s) <- c("Open", "High", "Low", "Close", "Volume")
# Plot OHLC data in x11 window
x11()
chart_Series(x=price_s, TA="add_Vo()",
             name="S&P500 ESF9 futures")
# Plot dygraph
library(dygraphs)
dygraphs::dygraph(price_s[, 1:4], main="S&P500 ESF9 futures") %>%
  dyCandlestick()



####################################
### Download market data from
### Interactive Brokers using package IBrokers.

# devtools::install_github(repo="joshuaulrich/IBrokers")
library(HighFreq)
library(IBrokers)

# Enable API Connections
# https://www.interactivebrokers.com/php/whiteLabel/globalConfig/configureApi.htm


## iBrokers trivial download script found somewhere
# Connect through Gateway and Windows
ib_connect <- IBrokers::ibgConnect(clientId=1, host='localhost', port=4002, verbose=TRUE, timeout=5, filename=NULL, blocking=.Platform$OS.type=="windows")
# Define a Ticker, Exchange and Source
con_tract <- IBrokers::twsEquity("AAPL", "SMART", "NYSE")
# Current market data
price_s <- IBrokers::reqMktData(ib_connect, con_tract)
# Real time tick bars
price_s <- IBrokers::reqRealTimeBars(ib_connect, con_tract)
# Loop to check data every 1 second
for (i in 1:60) {
  # Call function to check for crossovers
  Sys.sleep(1)
}  # end loop
# Disconnect from the Gateway
twsDisconnect(ib_connect)



###########
## Download raw data for replay, and then replay it

# define S&P Emini futures March 2019 contract
snp_contract <- twsFuture(symbol="ES",
  exch="GLOBEX", expiry="201909")
# define VIX futures March 2019 contract
vix_contract <- twsFuture(symbol="VIX",
  local="VXZ8", exch="CFE", expiry="201909")
# define 10yr Treasury futures March 2019 contract
trs_contract <- twsFuture(symbol="ZN",
  exch="ECBOT", expiry="201909")
# define Emini gold futures March 2019 contract
gold_contract <- twsFuture(symbol="YG",
  exch="NYSELIFFE", expiry="201909")
# define euro currency future March 2019 contract
euro_contract <- twsFuture(symbol="EUR",
  exch="GLOBEX", expiry="201909")
reqContractDetails(conn=ib_connect, Contract=euro_contract)

# define data directory
data_dir <- "C:/Develop/data/ib_data"
# dir.create(data_dir)

# open file for error messages
file_root <- "replay"
file_name <- file.path(data_dir, paste0(file_root, "_error.csv"))
error_connect <- file(file_name, open="w")

# open file for raw data
file_name <- file.path(data_dir, paste0(file_root, "_raw.csv"))
raw_connect <- file(file_name, open="w")

# create empty eWrapper to redirect error messages to error file
error_ewrapper <- eWrapper(debug=NULL, errfile=error_connect)

# create eWrapper for raw data
raw_ewrapper <- eWrapper(debug=TRUE)

# redirect error messages to error eWrapper (error_ewrapper),
# by replacing handler function errorMessage() in raw_ewrapper
raw_ewrapper$errorMessage <- error_ewrapper$errorMessage

# connect to Interactive Brokers TWS
ib_connect <- twsConnect()
# or connect to paper trading account
ib_connect <- twsConnect(port=7497)

# download raw data for multiple contracts for replay
reqMktData(ib_connect,
  list(snp_contract, vix_contract, trs_contract, gold_contract, euro_contract),
  eventWrapper=raw_ewrapper, file=raw_connect)

# close the Interactive Brokers API connection
twsDisconnect(ib_connect)

# close data files
close(raw_connect)
close(error_connect)



## replay the raw data

# open file with raw data
file_name <- file.path(data_dir, paste0(file_root, "_raw.csv"))
raw_connect <- twsConnect(file_name)
class(raw_connect) <- c("twsPlayback", class(raw_connect))
# replay the raw data
reqMktData(raw_connect, list(snp_contract, vix_contract))

# open file for data
file_connect <- file(file.path(data_dir, "temp.csv"), open="w")
# download TAQ data to file
reqMktData(conn=raw_connect,
           Contract=snp_contract,
           eventWrapper=eWrapper.MktData.CSV(1),
           file=file_connect)

# download bar to file
reqRealTimeBars(conn=raw_connect,
                Contract=snp_contract,
                barSize="1",
                eventWrapper=eWrapper.RealTimeBars.CSV(1),
                file=file_connect)

# Close file for bar data
close(file_connect)
# Close file with raw data
twsDisconnect(raw_connect)




###########
## Download daily historical OHLC prices and volume for SPX Index and XLU ETF
# ib_symbols <- "SPX Index"
ib_symbols <- c("SPX Index", "XLU US Equity")
ib_fields <- c("Open", "High", "Low", "Close", "Volume")
ib_fields <- c("BID", "ASK")
start_date <- as.Date("2017-08-01")

file_names <- file.path(data_dir,
  paste0(gsub(ib_symbols, pattern=" ", replacement="_"), ".csv"))


# define equity contracts
# con_tract <- IBrokers::twsContract(conId=265598, symbol="AAPL", sectype="Stock", exch="SMART", primary="NASDAQ", expiry="", strike=0, currency="USD", right="", local="AAPL", multiplier="")
sym_bol <- "QQQ"
sym_bol <- "AAPL"
con_tract <- IBrokers::twsEquity(sym_bol)
con_tract <- IBrokers::twsEquity(sym_bol, exch="SMART", primary="NASDAQ")
# con_tract <- IBrokers::twsEquity("QQQQ", "SMART", "ISLAND")
# con_tract <- IBrokers::twsFuture("YM", "ECBOT", "200809")

# define currency contracts
sym_bol <- "CHF"
# con_tract <- IBrokers::twsCurrency(sym_bol)
con_tract <- IBrokers::twsCurrency(sym_bol, currency="USD")

# define futures contracts
# ESU8 is September 2018 S&P Emini futures
sym_bol <- "ES"
con_tract <- IBrokers::twsFuture(symbol=sym_bol, exch="GLOBEX", expiry="201809")
con_tract <- IBrokers::twsFuture(symbol=sym_bol, local="ESU8", exch="GLOBEX", expiry="201809")
# TYU8 is September 2018 10yr Treasury futures
sym_bol <- "ZN"
con_tract <- IBrokers::twsFuture(symbol=sym_bol, exch="ECBOT", expiry="201809")
con_tract <- IBrokers::twsFuture(symbol=sym_bol, local="ZN   SEP 18", exch="ECBOT", expiry="201809")
# VXU8 is September 2018 VIX futures
sym_bol <- "VIX"
con_tract <- IBrokers::twsFuture(symbol=sym_bol, exch="CFE", expiry="201809")
con_tract <- IBrokers::twsFuture(symbol=sym_bol, local="VXU8", exch="CFE", expiry="201809")
# GCN8 is July 2018 Gold futures
sym_bol <- "GC"
con_tract <- IBrokers::twsFuture(symbol=sym_bol, exch="NYMEX", expiry="201807")
con_tract <- IBrokers::twsFuture(symbol=sym_bol, local="GCN8", exch="NYMEX", expiry="201807")



IBrokers::is.twsContract(con_tract)


# open file for data download
# foo <- file.create(file_name)
data_dir <- "C:/Develop/data/ib_data"
file_name <- file.path(data_dir, paste0(sym_bol, ".csv"))
file_connect <- file(file_name, open="w")


# connect to Interactive Brokers TWS
ib_connect <- IBrokers::twsConnect()
# or connect to IB Gateway
ib_connect <- IBrokers::ibgConnect()

# get info
IBrokers::isConnected(ib_connect)
IBrokers::serverVersion(ib_connect)

ib_time <- IBrokers::reqCurrentTime(ib_connect)
lubridate::tz(ib_time)
ib_time <- (ib_time - 6*3600)
ib_time <- paste0(gsub(pattern="-", replacement="", ib_time), " EDT")


# download list with instrument information
contract_info <- IBrokers::reqContractDetails(conn=ib_connect, Contract=con_tract)
contract_info <- contract_info[[1]]
contract_info$longName
IBrokers::reqContractDetails(conn=ib_connect,
                             Contract=IBrokers::twsFuture("VIX", "CFE", "201809"))

IBrokers::reqContractDetails(conn=ib_connect, Contract=IBrokers::twsEquity("AAPL"))


# download data to file
foo <- IBrokers::reqHistoricalData(conn=ib_connect,
                            Contract=con_tract,
                            # whatToShow="MIDPOINT",
                            # endDateTime=ib_time,
                            barSize="1 day",
                            duration="6 M",
                            file=file_connect)

foo <- IBrokers::reqHistory(conn=ib_connect,
                            Contract=con_tract,
                            barSize="1 min")


IBrokers::reqHistoricalData(conn=ib_connect,
                            Contract=con_tract,
                            endDateTime=Sys.time(),
                            barSize="1 min",
                            duration="1 M",
                            file=file_connect)

IBrokers::reqMktData(conn=ib_connect,
                     twsEquity("AAPL"),
                     eventWrapper=eWrapper.MktData.CSV(1),
                     file=file_connect)

IBrokers::reqRealTimeBars(conn=ib_connect,
                     twsEquity("AAPL"),
                     eventWrapper=eWrapper.MktData.CSV(1),
                     file=file_connect)

# close data file
close(file_connect)
# close the Interactive Brokers API connection
IBrokers::twsDisconnect(ib_connect)



## Load time series data from a single csv file

price_s <- read.csv(file=file_name,
                    stringsAsFactors=FALSE, sep=",",
                    header=FALSE)

price_s <- xts::xts(price_s[, -1],
  order.by=as.POSIXct.numeric(price_s[, 1], tz="America/New_York", origin="1970-01-01"))
colnames(price_s)[1:5] <- ib_fields
price_s <- price_s[, 1:5]


# overwrite NA values
price_s <- rutils::na_locf(price_s)
price_s <- rutils::na_locf(price_s, from_last=TRUE)
sum(is.na(price_s))
dygraphs::dygraph(price_s[, 1:4], main="OHLC prices") %>% dyCandlestick()



# ib_data <- bdh(securities=ib_symbols,
#                 fields=ib_fields,
#                 start.date=start_date)

# download data from Interactive Brokers in loop
lapply(seq_along(ib_symbols), function(in_dex) {
  sym_bol <- ib_symbols[in_dex]
  ib_data <- xts::as.xts(Rblpapi::bdh(securities=sym_bol,
                                       fields=ib_fields,
                                       start.date=start_date))
  file_name <- file.path(data_dir,
                         paste0(gsub(sym_bol, pattern=" ", replacement="_"), ".csv"))
  zoo::write.zoo(ib_data, file=file_name, sep=",")
  sym_bol
})  # end lapply


###########
### Load data from IB in loop

# define VIX futures parameters
sym_bol <- "VIX"
ex_change <- "CFE"

year_s <- c(rep("7", 6), rep("8", 8))
month_s <- paste0("201", year_s, formatC(c(7:12, 1:8), width=2, flag="0"))
month_codes <- c("N", "Q", "U", "V", "X", "Z", "F", "G", "H", "J", "K", "M", "N", "Q")

sapply(1:NROW(month_codes), function(it) {
  lo_cal <- paste0("VX", month_codes[it], year_s[it])
  con_tract <- twsFuture(symbol=sym_bol,
                         include_expired="1",
                         local=lo_cal,
                         exch=ex_change, expiry=month_s[it])
  file_name <- file.path(data_dir, paste0(lo_cal, ".csv"))
  file_connect <- file(file_name, open="w")
  reqHistoricalData(conn=ib_connect,
                    Contract=con_tract,
                    barSize="1 day", duration="2 Y",
                    file=file_connect)
  # close data file
  close(file_connect)
})  # end sapply



###


###########
### Load data from csv files

## Load time series data from a single csv file

price_s <- xts::as.xts(zoo::read.zoo(
  file=file.path("C:/Develop/data",
                 "data prices close 2017-08-31.csv"),
  header=TRUE, sep=",", FUN=as.Date, format="%m/%d/%Y"))
# overwrite NA values
price_s <- rutils::na_locf(price_s)
price_s <- rutils::na_locf(price_s, from_last=TRUE)
symbol_s <- c("XLP", "XLU")
price_s <- price_s[, symbol_s]



## Load time series data from csv files into an environment.

# create new environment for data
data_env <- new.env()
data_dir <- "C:/Develop/data/ib_records"
# sym_bols <- c("SPX", "VIX")
# file_names <- paste0(sym_bols, ".csv")
file_names <- dir(data_dir)
sym_bols <- rutils::get_name(file_names)

# subset sym_bols by removing currency symbols
sub_symbols <- sym_bols[-grep("USD", sym_bols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("EUR", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("UST", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("JGB", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("GDB", sub_symbols, ignore.case=TRUE)]


# load data from csv files into the environment
out <- rutils::get_data(sym_bols=sub_symbols,
                        data_dir=data_dir,
                        data_env=data_env,
                        e_cho=FALSE)


## Extract the closing prices into a single xts time series

# price_s <- lapply(as.list(data_env)[sym_bols], quantmod::Cl)
# flatten (cbind) prices into single xts series
# price_s <- rutils::do_call(cbind, price_s)

price_s <- rutils::get_col(oh_lc=ls(data_env),
                           data_env=data_env)
# overwrite NA values
price_s <- rutils::na_locf(price_s)
price_s <- rutils::na_locf(price_s, from_last=TRUE)
# save column names
col_names <- rutils::get_name(colnames(price_s))




###############
### Load and save OHLC bar data

library(HighFreq)

## Load ES1 futures data from binary file
load(file="C:/Develop/data/ES1.RData")
# or
# load ES1 futures data from CSV file
oh_lc <- read.zoo(file="C:/Develop/data/bar_data/ES1.csv",
                  header=TRUE, sep=",",
                  drop=FALSE, format="%Y-%m-%d %H:%M",
                  FUN=as.POSIXct, tz="America/New_York")
# coerce to xts series
oh_lc <- as.xts(oh_lc)
# subset to trading hours
oh_lc <- oh_lc["T09:00:00/T16:30:00"]
# save the bar data to binary file
save(oh_lc, file="C:/Develop/data/ES1.RData")


## Load futures data from CSV files

# read file names
file_names <- scan(file="C:/Develop/data/bar_data/etf_file_names.txt", what=character(), sep=",")

# remember the cwd
c_wd <- getwd()
# set the cwd to the file directory
file_dir <- strsplit(file_names[1], split="/")[[1]]
file_dir <- file_dir[-NROW(file_dir)]
file_dir <- paste(file_dir, collapse="/")
# or
# file_dir <- do.call(file.path, as.list(file_dir))
setwd(dir=file_dir)

# loop over the file_names, load data from CSV files,
# and save the bar data to binary files
for (file_name in file_names) {
  file_name <- strsplit(file_name, split="/")[[1]]
  file_name <- file_name[NROW(file_name)]
  # load time series data from CSV file
  oh_lc <- read.zoo(file=file_name,
                    header=TRUE, sep=",",
                    drop=FALSE, format="%Y-%m-%d %H:%M",
                    FUN=as.POSIXct, tz="America/New_York")
  # coerce to xts series
  oh_lc <- as.xts(oh_lc)
  sym_bol <- strsplit(file_name, split="[.]")[[1]][1]
  # rename column names
  colnames(oh_lc) <- paste(sym_bol, colnames(oh_lc), sep=".")
  # subset to trading hours
  # oh_lc <- oh_lc["T09:00:00/T16:30:00"]
  # save the bar data to binary file
  save(oh_lc, file=paste0(sym_bol, ".RData"))
}  # end for

# restore the cwd
setwd(dir=c_wd)


## Load futures data from RData files

# read the symbols
sym_bols <- scan(file="C:/Develop/data/bar_data/etf_symbols.txt", what=character(), sep=",")
# specify the file directory
file_dir <- "C:/Develop/data/bar_data/"
# specify new environment for data
etf_env <- new.env()
# specify the file names
# file_names <- paste0(file_dir, sym_bols, ".RData")

# load data in a loop and copy into etf_env
for (sym_bol in sym_bols) {
  # specify the file name
  file_name <- paste0(file_dir, sym_bol, ".RData")
  load_ed <- load(file=file_name)
  assign(x=sym_bol, value=get(load_ed), envir=etf_env)
}  # end for


## Combine the ETF series of prices into a single xts series and save it into etf_env

# extract only first 4 OHLC price columns from each ETF series
assign(x="oh_lc",
       value=rutils::do_call(cbind, eapply(etf_env, function(x_ts) x_ts[, 1:4])),
       envir=etf_env)
# oh_lc <- rutils::do_call(cbind, eapply(etf_env, function(x_ts) x_ts[, 1:4]))
etf_env$oh_lc <- na.omit(etf_env$oh_lc)
# subset to trading hours
etf_env$oh_lc <- etf_env$oh_lc["T09:00:00/T16:30:00"]
# save the bar data to binary file
save(etf_env, file=paste0(file_dir, "etf_series.RData"))



## Load futures data from binary files and combine into a single xts series
# first load ES1 data and extract only first 4 OHLC price columns
load(file="C:/Develop/data/ES1.RData")
com_bo <- oh_lc[, 1:4]
colnames(com_bo) <- paste0("ES1.", colnames(com_bo))
# next load TU1 data and cbind it to ES1 data
load(file="C:/Develop/data/TU1UST2yr.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("TU1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
# next load TY1 data and cbind it to ES1 data
load(file="C:/Develop/data/TY1UST10yr.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("TY1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
# next load UX1 data and cbind it to ES1 data
load(file="C:/Develop/data/UX1_VIX.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("UX1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
load(file="C:/Develop/data/UX2_VIX.RData")
oh_lc <- oh_lc[, 1:4]
# next load UX1 data and cbind it to ES1 data
colnames(oh_lc) <- paste0("UX2.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)

# combine into a single xts series
oh_lc <- na.omit(com_bo)
# save the bar data to binary file
save(com_bo, file="C:/Develop/data/combined.RData")
# load(file="C:/Develop/data/combined.RData")

# plot dygraph
label_s <- c("TY1.Close", "TU1.Close")
# dygraphs::dygraph(cbind(clo_se, da_ta())["2018-02-09"], main="OHLC Technicals Strategy") %>%
dygraphs::dygraph(oh_lc[endpoints(oh_lc, on="hours"), label_s], main="OHLC Data") %>%
  dyAxis("y", label=label_s[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=label_s[2], independentTicks=TRUE) %>%
  dySeries(label_s[2], axis="y2", col=c("blue", "red"))



########### very weird
## iBrokers trading script from:
# https://www.quantopian.com/posts/ib-api

# /a/ks/spy25/actonit.r

# I use this script to act on predictions written to this file:
# /a/ks/spy25/data/prediction.csv
# The tail end of the csv looks like this:
# 1414625400 197.835 0.594 0.586

# I want to be long or short by this amount:
possize = 3

arow = read.csv('data/prediction.csv', sep=' ', header=FALSE)

ptime = as.integer(arow[1][1])
print(ptime)
etime = as.POSIXct(ptime, origin="1970-01-01")
print(etime)

timediff_minutes = as.integer(difftime(Sys.time(), etime, units = "mins"))

myprediction = arow[4]

if (myprediction > 0.5 & timediff_minutes < 6) {
  print('buy')
}

if (myprediction < 0.5 & timediff_minutes < 6) {
  print('sell')
}

# Now I make use of the IBrokers package.
# Syntax to install it:
# install.packages("IBrokers", lib="rpackages", repos="http://cran.us.r-project.org")
# Once I install it, I see it in a folder named rpackages/

# Next, tell this script where IBrokers resides:
.libPaths("rpackages")

# Now I can use IBrokers R Package:
library(IBrokers)

myport     = 7476
myclientId = 2

tws2 = twsConnect(clientId=myclientId, port=myport)
Sys.sleep(2)

# Order if position small:
myacct = reqAccountUpdates(tws2)
myposition = myacct[[2]][[1]]$portfolioValue$position
# myposition = 1
print(myposition)
twsDisconnect(tws2)

Sys.sleep(10)

tws2 = twsConnect(clientId=myclientId, port=myport)

mytkr     = twsFuture("ES","GLOBEX","201412")
Sys.sleep(2)
myorderid = as.integer(reqIds(tws2))
print(myorderid)
Sys.sleep(2)
myorderid = as.integer(difftime(Sys.time(), "2014-10-30", units = "secs"))

if (myposition == -possize & myprediction > 0.5 & timediff_minutes < 6) {
  print('Attempting BUY')
  IBrokers:::.placeOrder(tws2, mytkr, twsOrder(myorderid,"BUY", 2*possize, "MKT"))
}

if (myposition == possize & myprediction < 0.5 & timediff_minutes < 6) {
  print('Attempting SELL')
  IBrokers:::.placeOrder(tws2, mytkr, twsOrder(myorderid,"SELL", 2*possize, "MKT"))
}

Sys.sleep(2)
twsDisconnect(tws2)



####################################
# ignore below


# coerce ib_data from data frame to xts
# ib_data <- xts::as.xts(ib_data)
# need to verify date format
ib_data <- xts::xts(ib_data[, ib_fields],
                     order.by=as.Date(ib_data[, "date"], format="%Y-%m-%d"))

# coerce ib_data from data frame to xts
# ib_data <- xts::as.xts(ib_data)
# ib_data <- xts::xts(ib_data, order.by=as.Date(ib_data[, "date"]))
# ib_data
# write ib_data to CSV file
zoo::write.zoo(ib_data, file=file_name, sep=",")


# write ib_data to CSV files
zoo::write.zoo(ib_data, file="ib_data.csv", sep=",")


## Download daily historical close prices and daily volume for SPX Index and XLU ETF



### Interactive Brokers script for a list of symbols





