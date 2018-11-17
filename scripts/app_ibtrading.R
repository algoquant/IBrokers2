##############################
# This is a shiny app for simulating a rolling portfolio 
# optimization strategy with filtering of returns.
# It uses HighFreq::back_test()
# 
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(IBrokers2)
# Model and data setup
# source the model function
# source("C:/Develop/R/lecture_slides/scripts/roll_portf_new.R")
# max_eigen <- 2

# Load the trading function written as an eWrapper:
source("C:/Develop/R/IBrokers2/R/trade_wrapper.R")


# Define the contract for trading
con_tract <- IBrokers2::twsFuture(symbol="ES", exch="GLOBEX", expiry="201812")

# Open the file for storing the bar data
data_dir <- "C:/Develop/data/ib_data"
file_name <- file.path(data_dir, paste0("ES_ohlc_live_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connect <- file(file_name, open="w")

# Open the IB connection
ib_connect <- IBrokers2::twsConnect(port=7497)



# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Trading via IBrokers2"),
  
  # create single row with two slider inputs
  fluidRow(
    column(width=3, sliderInput("buy_spread", label="buy spread:",
                                min=0.0, max=2.0, value=0.25, step=0.25)),
    column(width=3, sliderInput("sell_spread", label="sell spread:",
                                min=0.0, max=2.0, value=0.25, step=0.25))
  )  # end fluidRow
)  # end fluidPage interface


## Define the server code
ser_ver <- function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    buy_spread <- input$buy_spread
    sell_spread <- input$sell_spread

    # Run the trading model (strategy):
    IBrokers2::reqRealTimeBars(conn=ib_connect, useRTH=FALSE,
                               Contract=con_tract, barSize="10",
                               eventWrapper=trade_wrapper(n_instr=1,
                                                          buy_spread=buy_spread, sell_spread=sell_spread),
                               CALLBACK=twsCALLBACK,
                               file=file_connect)
    1
  })  # end reactive code
  

}  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
