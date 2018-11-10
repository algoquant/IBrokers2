############
### ToDo list for package IBrokers2: Executing Real Time Trading Strategies Via the API of Interactive Brokers

### scripts for package building

## Steps for installing package called my_package, containing Rcpp code:
1. Create empty package directory with Rcpp::Rcpp.package.skeleton()
2. In the R directory create a file called my_package.R and add the header:

#' @useDynLib my_package
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"
NULL

Add your R functions below the header.

3. Clean and rebuild (in Build menu)


## Build scripts:

# Install package from source on local drive
install.packages(pkgs="C:/Develop/R/IBrokers2", repos=NULL, type="source")
# Install package from source on local drive using R CMD
R CMD INSTALL C:\Develop\R\IBrokers2
# Install package from github
devtools::install_github(repo="algoquant/IBrokers2", force=TRUE)
# build vignette package reference manual from *.Rd files
system("R CMD Rd2pdf C:/Develop/R/IBrokers2")
cd C:\Develop\R\IBrokers2\vignettes
R CMD Rd2pdf C:\Develop\R\IBrokers2\



### Comments and package analysis

# List of numeric codes of commands stored as a named list
IBrokers::.twsIncomingMSG


### tasks to-do

+ [x] Download data for two contracts simultaneously

+ [x] Change the trading frequency - realtimeBars query in IB API is fixed to 5 seconds

+ [x] In realtimeBars() add to col_index the instrument ID column

+ [x] Pass into IBrokers2::reqRealTimeBars() a named list of contracts called con_tracts (even for a single contract)

+ [x] In trade_wrapper(), pass a vector of contract (instrument) names called name_s, instead of the integer n_instr
Add column names to bar_data.
Add argument file_connects, and write the column names as headers to data files.
Remove the dots argument of trade_wrapper().

+ [ ] Print to console status of the eWrapper data buffer

+ [ ] Create clone of reqRealTimeBars() called trade_realtime()

+ [ ] Create clone of twsCALLBACK() called call_back(), 
Adapt from:
http://r.789695.n4.nabble.com/Howto-cancel-reqMktData-from-IBrokers-package-td1562054.html

+ [ ] Load into buffer the state variables: size of open orders, positions, cumulative PnL
IBrokers2::reqOpenOrders
function(twsconn) {
  .reqOpenOrders(twsconn)
  con <- twsconn[[1]]
  eW  <- eWrapper()
  while(TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1L)
    processMsg(curMsg, con, eW)
  }
}

+ [ ] In realtimeBars() check for trade status using reqOpenOrders() instead of copying tradeID
https://stackoverflow.com/questions/34703679/r-ibrokers-reqopenorders-hangs

+ [x] Download real-time bars for multiple instruments using reqRealTimeBars() and eWrapper.RealTimeBars.CSV(), and compare the data

+ [ ] Every 10 counts, save all the bar_data, instead of every row, or save only 10 rows of bar_data at a time to a file, instead of every row. 

+ [ ] Create sub-portfolios and place trades into sub-portfolios

+ [ ] Download messages and parse them with reqMktData(tws, equity1, CALLBACK=NULL, file=fn)
http://r.789695.n4.nabble.com/Interactive-Brokers-td3668170.html

+ [ ] Create new order type:
https://stackoverflow.com/questions/46482300/r-ibrokers-interactive-brokers-api

+ [ ] Run reqExecutions():
https://stackoverflow.com/questions/35559742/reqexecutions-ibrokers-package
http://r.789695.n4.nabble.com/IBrokers-order-status-td4562387.html
http://r.789695.n4.nabble.com/Confirm-orders-in-TWS-Interactive-Brokers-td3785506.html

+ [ ] Adapt iBrokers code from:
https://www.quantopian.com/posts/ib-api
https://stackoverflow.com/questions/35559742/reqexecutions-ibrokers-package
http://r.789695.n4.nabble.com/Confirm-orders-in-TWS-Interactive-Brokers-td3785506.html

+ [ ] Answer the questions:
https://stackoverflow.com/questions/39713543/message-management-using-ewrapper-and-twscallback-in-ibrokers
https://stackoverflow.com/questions/27254131/ibrokers-queueing-up-an-order-using-r

+ [ ] Reply to / comment on:
https://stackoverflow.com/questions/44582053/ibrokers-quantstrat-live-implementation

+ [ ] Answer the question:
https://stackoverflow.com/questions/21442158/ibrokers-data-persistence

+ [ ] Add new order type to IBrokers:
https://stackoverflow.com/questions/46482300/r-ibrokers-interactive-brokers-api

+ [ ] Adapt from autotrade by Phillip Guerra: uses package reticulate with Python for Interactive Brokers
https://www.pcppresentation.com/
https://github.com/PhilGuerra/autotrade
https://github.com/PhilGuerra/autotrade/commits?author=PhilGuerra
Guerra autotrade RFinance Chicago 2018.pdf
C:/Research/R/Tutorials/Guerra_automated_trading_with_r

+ [ ] Adapt from Santosh Srinivas: Interactive Brokers with Python, R and package reticulate
https://www.santoshsrinivas.com/amicable-interactive-data-analysis-using-pythoth-and-r-using-reticulate/


### tasks finished


+ [x] In realtimeBars() of trade_wrapper(): copy bar data into buffer in the event wrapper environment

+ [x] Convert bar_data into list of matrices or data frames, instead of a list of xts series

+ [x] Rewrite trade_wrapper()
Rename Wrapper_new() to create_ewrapper().
Remove redundant accessor function calls.
Define model function inside trade_wrapper() and pass model parameters into trade_wrapper() through the dots argument.

+ [x] Create IBrokers2 project directory in RStudio

+ [x] Pass the trading parameters buy_spread and sell_spread into the dots of realtimeBars()

+ [x] Adapt Interactive Brokers API script from: actually nothing there to adapt
C:\Develop\R\IBrokers\Sherrington IBrokers scripts.pdf
C:\Develop\R\IBrokers\Yadav IBrokers scripts.pdf




### tasks deprecated

