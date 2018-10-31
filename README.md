[![Build Status](https://travis-ci.org/algoquant/IBrokers2.svg?branch=master)](https://travis-ci.org/algoquant/IBrokers2)

### Overview

The package *IBrokers2* is derived from, and is a superset of the package *IBrokers*. This means that all the *IBrokers* functions and variables are preserved exactly in *IBrokers2*, while some additional functions have been added. The new functions mostly provide additional trade execution capabilities, for running systematic trading strategies in a callback loop.

### Installation and loading

Install package *IBrokers2* from github:

``` r
install.packages("devtools")
devtools::install_github(repo="algoquant/IBrokers2")
library(IBrokers2)
```

<br>

### Implementation details

The original function *IBrokers::reqRealTimeBars()* is used for running trading strategies in a callback loop, by providing to it a customized *event wrapper* function, for example *IBrokers2::trade\_wrapper()*. The function *IBrokers2::trade\_wrapper()* is derived from the function *IBrokers::eWrapper.RealTimeBars.CSV()*. It creates a wrapper environment, defines the member function *realtimeBars()*, and returns the wrapper environment. The function *realtimeBars()* is where the trading code resides, for updating a model and then executing trades based on it.

The function *IBrokers::reqRealTimeBars()* calls the functions *IBrokers::twsCALLBACK()* and *IBrokers2::trade\_wrapper()*. It then passes *trade\_wrapper()* into *IBrokers::twsCALLBACK()*. This way the wrapper environment created by *IBrokers2::trade\_wrapper()* persists in the evaluation environment of the function *twsCALLBACK()*, in between bar data arrivals as a *mutable state*.

The callback loop is performed inside the function *IBrokers::twsCALLBACK()*, which calls *IBrokers::processMsg()* in a loop, which in turn calls *realtimeBars()* (defined inside *IBrokers2::trade\_wrapper()*).

An example of a simple trading strategy can be run with the code in the file *IB\_scripts.R* in the *scripts* sub-directory. The user can customize this strategy by modifying the trading code in *realtimeBars()*.
