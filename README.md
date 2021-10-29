[![Build Status](https://travis-ci.org/algoquant/IBrokers2.svg?branch=master)](https://travis-ci.org/algoquant/IBrokers2)

### Overview

The package *IBrokers2* contains *R* functions for executing real-time trading strategies via the <a href="https://interactivebrokers.github.io/tws-api/" target="_blank">API of Interactive Brokers (IB API)</a>. The package *IBrokers2* is derived from package <a href="https://cran.r-project.org/web/packages/IBrokers/index.html" target="_blank"><em>*IBrokers*</em></a>, and is fully backward compatible with it. This means that all the *IBrokers* functions and variables are preserved exactly in *IBrokers2*, while some additional functions have been added. The new functions mostly provide additional trade execution capabilities, for running systematic trading strategies in a callback loop.

More information can be found in the document *real\_time\_trading.html* in the vignettes sub-directory.

### Installation and Loading

Install package *IBrokers2* from github:

``` r
if (!("package:devtools" %in% search() || require("devtools", quietly=TRUE)))
    install.packages("devtools")
devtools::install_github(repo="algoquant/IBrokers2")
library(IBrokers2)
```

<br>
