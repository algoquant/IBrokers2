[![Build Status](https://travis-ci.org/algoquant/IBrokers2.svg?branch=master)](https://travis-ci.org/algoquant/IBrokers2)

### Overview

The package *IBrokers2* is derived from, and is a superset of the package *IBrokers*. This means that all the *IBrokers* functions and variables are preserved exactly in *IBrokers2*, while some additional functions have been added. The new functions mostly provide additional trade execution capabilities, for running systematic trading strategies in a callback loop.

### Installation and Loading

Install package *IBrokers2* from github:

``` r
install.packages("devtools")
devtools::install_github(repo="algoquant/IBrokers2")
library(IBrokers2)
```

<br>

More information can be found in the document *real\_time\_trading.html* in the vignettes sub-directory.
