.IBrokersEnv <- new.env()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("IBrokers2 version 0.1.  Implementing API Version 9.64")
  packageStartupMessage("\nIBrokers2 comes with NO WARRANTY.  NOT INTENDED FOR LIVE TRADING!\n\n")
  packageStartupMessage("See ?IBrokers2 for details.")
}

.onLoad <- function(libname,pkgname) {
}

