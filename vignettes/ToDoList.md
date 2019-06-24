############
### ToDo list for package IBrokers2: Executing Real Time Trading Strategies Via the API of Interactive Brokers

### Scripts for building packages

## Steps for creating a package called my_package, which contains Rcpp code:
1. Create empty package directory with the R command: Rcpp::Rcpp.package.skeleton()
2. In the R sub-directory of the new package, create a file called my_package.R and add the header:
	#' @useDynLib my_package
	#' @importFrom Rcpp evalCpp
	#' @exportPattern "^[[:alpha:]]+"
Add your R functions below the above header.
3. In the Build menu, Clean and rebuild


## Scripts for installing packages:

Install package from source on local drive
	install.packages(pkgs="C:/Develop/R/IBrokers2", repos=NULL, type="source")
Install package from source on local drive using R CMD
	R CMD INSTALL C:\Develop\R\IBrokers2
Install package from github
	devtools::install_github(repo="algoquant/IBrokers2", force=TRUE)
Build vignette package reference manual from *.Rd files
	system("R CMD Rd2pdf C:/Develop/R/IBrokers2")
	cd C:\Develop\R\IBrokers2\vignettes
	R CMD Rd2pdf C:\Develop\R\IBrokers2\



### Comments and package analysis

+ The parameter trade_params is passed into the function e_wrapper$model_fun(), and it isn't used outside it  

+ Currently the design of e_wrapper$realtimeBars() only allows to trade a single instrument at a time

+ The numeric IB API message codes are stored as named lists:
IBrokers::.twsIncomingMSG
IBrokers::.twsOutgoingMSG

+ In e_wrapper$realtimeBars(), when would this ever be true: is.null(trade_params) ?  Why is it needed?
Answer: It is true for instruments which aren't traded, because they have trade_params equal to NULL



### Notes

+ [ ] How to open IB demo account
https://www.interactivebrokers.com/en/index.php?f=1286

+ [ ] How to trade IB using Python configure IB account
https://medium.com/auquan/algorithmic-trading-system-development-1a5a200af260



### tasks to-do

+ [x] In e_wrapper, call model_fun() using indirection using the name of the model function from the list trade_params
Add the name of the model function to the list trade_params.

+ [ ] Create vignette for package IBrokers2  

+ [ ] Add software disclaimer

+ [ ] Modify the function e_wrapper$realtimeBars() to be able to trade several instruments simultaneously  
The function e_wrapper$model_fun() would require access to the trade_params parameter of several instruments.

+ [ ] In trade_wrapper() add limit on position inventory: if inventory reaches its limit then stop placing orders in that direction
Add lim_it to the list trade_params in trade_wrapper().

+ [ ] In realtimeBars() calculate the trailing VWAP, volatilities, and z-scores

+ [ ] In IB_scripts.R use limit order with GoodTillDate order expiration, instead of canceling the order
https://interactivebrokers.github.io/tws-api/classIBApi_1_1Order.html#a95539081751afb9980f4c6bd1655a6ba

+ [ ] Create a clone of reqOpenOrders() called get_open_orders() to write to data buffer

+ [ ] In realtimeBars() check for trade status using reqOpenOrders() instead of copying tradeID
https://stackoverflow.com/questions/34703679/r-ibrokers-reqopenorders-hangs

+ [ ] In create_ewrapper() modify the handlers openOrder() and openOrderEnd() for reqExecutions and reqOpenOrders

+ [ ] Load into eWrapper buffer the state variables: size of open orders, positions, cumulative PnL
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

+ [ ] Find out how to book iBrokers trades in different models (portfolios)
https://www.interactivebrokers.com/en/software/tws/usersguidebook/modelportfolios/createmodel.htm

+ [ ] Adapt from IB script using R6Class: C:/Develop/R/IBrokers2/scripts/TWS Kovalevsky.R

+ [ ] Demonstrate how to use iBrokers data playback replay feature
The script added to data_management.Rnw downloads the raw data, but doesn't replay the bar data properly.
https://offerm.wordpress.com/2015/05/21/market-data-recording-and-playback-with-ibrokers-and-r-2/

+ [ ] Adapt from: downloading raw market data using IBrokers::reqMktData() without eventWrapper
https://stat.ethz.ch/pipermail/r-sig-finance/2011q3/008232.html

+ [ ] Print to console status of the eWrapper data buffer

+ [ ] In create_ewrapper() modify openOrder() to write to the da_ta environment and to a file

+ [ ] Create a clone of reqAccountUpdates() called get_account()

+ [ ] Create sub-portfolios and place trades into sub-portfolios: use modelCode ?
https://www.interactivebrokers.com/en/software/tws/usersguidebook/mosaic/portfoliobuilder.htm
interactivebrokers sub portfolios

+ [ ] Create a shiny app as a front end for trading via IBrokers2: 
C:\Develop\R\IBrokers2\scripts\app_ibtrading.R
Every time model parameters are updated, the shiny app should interrupt the trading model in R and then restart it with the new parameters.
Use on.exit() to remember the trade IDs.
I've been trying to find a solution for this problem, but only stumbled into vague mentions.
The goal is to receive live data from a trading platform with IBrokers package, accumulate it, do computations with it periodically and as the user changes reactive inputs.
The package uses subscribe-callback mechanism to process new data. Once the data is requested, the function goes to a while(TRUE) loop and passes incoming messages to a callback function, where it can be written to a data frame, until stopped. Although it is not computationally intensive, it occupies the session.
Is it possible to update the data on a background and periodically do the calculations with it? I suspect it somehow involves multiple R sessions, since R is single threaded. I would appreciate any tips.
https://groups.google.com/forum/#!topic/shiny-discuss/n11-mnBYXQc
https://github.com/ksavin/intrinio
https://www.linkedin.com/in/ksavin/
https://gist.github.com/trestletech/8608815
https://stackoverflow.com/questions/21282228/update-plot-within-observer-loop-in-shiny-application

+ [ ] Remove Depends: and Imports: from DESCRIPTION ?

+ [ ] Modify IBrokers2 startup message in zzz.R

+ [ ] Add sounds when trades are placed
https://stackoverflow.com/questions/3365657/is-there-a-way-to-make-r-beep-play-a-sound-at-the-end-of-a-script
https://shirinsplayground.netlify.com/2018/06/text_to_speech_r/
https://sourceforge.net/projects/espeak/files/espeak/
http://code.markedmondson.me/googleLanguageR/articles/text-to-speech.html
https://github.com/seankross/ari
cd C:\Program Files (x86)\espeak\command_line
espeak.exe -v english-us -s 100 "Buy"

+ [ ] Every 10 counts, save all the bar_data, instead of every row, or save only 10 rows of bar_data at a time to a file, instead of every row. 

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

+ [x] In e_wrapper$model_fun() introduce position limits using e_wrapper$da_ta$position[contract_id]
Add position limit to the list trade_params.

+ [x] Remove old file eWrapper_trading.R (old version of trade_wrapper.R)  

+ [x] In e_wrapper$realtimeBars(), update EWMA and volatility for all instruments, even for non-traded instruments

+ [x] Set trade_params to NULL for non-traded instruments, instead of NA

+ [x] Fix bug in e_wrapper$realtimeBars: catch case when ib_account is NULL, for example at initial state when there are no positions  

+ [x] In trade_wrapper(), rename variable sprea_d to bia_s

+ [x] Pass ac_count argument with acctCode into trade_wrapper()

+ [x] Call reqAccountUpdates() inside realtimeBars() to download net positions from IB

+ [x] In realtimeBars() and model_fun() add argument ib_connect
In processMsg() pass argument twsconn into eWrapper$realtimeBars()

+ [x] In trade_realtime() rename the argument playback to back_test (with default FALSE), for backtesting trading strategies using 5-second bar data

+ [x] Create a clone of twsCALLBACK() called call_back() - rename argument playback to back_test, with default FALSE

+ [x] Move argument lamb_da to argument vector trade_params

+ [x] In trade_wrapper() add spread bias to the limit prices if there is momentum
For example, use EWMA crossover.
Or if there are two consecutive trades in the same direction.

+ [x] In trade_wrapper() add argument warm_up for warmup period: don't trade in warmup period
Rename argument fac_tor to warm_up.

+ [x] Add argument trade_params to model_fun()

+ [x] In trade_wrapper() get rid of limit_prices

+ [x] In trade_wrapper() add ability to trade with a time lag
Create a matrix of past limit_prices, and update the limit_prices in model_fun().
Set the current limit prices to past limit_prices.

+ [x] Rename limit_prices to trade_params, and add lagg parameter to trade_params

+ [x] Increase TWS Java heap size to 2.5 GB in file C:\Jts\tws.vmoptions
-Xmx2500m
https://www.interactivebrokers.com/en/software/tws/usersguidebook/priceriskanalytics/custommemory.htm
https://ibkr.info/article/2170

+ [x] In trade_wrapper() add spread factor for limit price, proportional to the trailing volatility

+ [x] Create clone of reqRealTimeBars() called trade_realtime()

+ [x] In create_ewrapper() rename .Data to da_ta

+ [x] Use assign() instead of "<<-" - actually revert back to "<<-"

+ [x] In trade_realtime() rename con to sock_et, and conn to ib_connect, and twsconn to ib_connect

+ [x] In create_ewrapper() rename assign.Data() to as_sign()

+ [x] Pass con_tracts and limit_prices arguments into trade_wrapper(), and pass contract_id argument into model_fun()
Trade only those contracts which have non-NA limit_prices.

+ [x] Download data for two contracts simultaneously

+ [x] Change the trading frequency - realtimeBars query in IB API is fixed to 5 seconds

+ [x] In realtimeBars() add to col_index the instrument ID column

+ [x] Pass into IBrokers2::reqRealTimeBars() a named list of contracts called con_tracts (even for a single contract)

+ [x] In trade_wrapper(), pass a vector of contract (instrument) names called name_s, instead of the integer n_instr
Add column names to bar_data.
Add argument file_connects, and write the column names as headers to data files.
Remove the dots argument of trade_wrapper().

+ [x] Download real-time bars for multiple instruments using reqRealTimeBars() and eWrapper.RealTimeBars.CSV(), and compare the data

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

