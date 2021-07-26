library(shiny)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(RColorBrewer)
library(tseries)
library(lubridate)
library(Quandl)
Quandl.api_key("zrcB2Ejv9UmvhPCUsy2_")
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

shinyUI(fluidPage(
    titlePanel("Cryptocurrency Analysis Project"),
    sidebarLayout(
        sidebarPanel(
            selectInput("name", "Select Cryptocurrency:", c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin")),
            sliderInput("year", "Select Time Range:", min = 2016, max = as.numeric(format(as.Date(Sys.Date()), '%Y'))+1, value = c(2018, 2020), sep = "", pre = "Year "),
            selectInput("tech_ind", "Select Technical Indicator:", c("Bollinger Bands", 
                                                                      "Exponential Moving Averages")),
            selectInput("price", "Select Price to be analysed:", c("Open", "Close")),
            actionButton("goButton1", "Chart!"),
            dateInput("date", "Enter the Prediction Date:", value = Sys.Date()+1),
            actionButton("goButton2", "Forecast!")
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Documentation", br(), strong("Select Cryptocurrency: "), "Select a currency from the given list of 10 to chart. The default is Bitcoin.",
                                 br(), strong("Select Time Range: "), "Choose for which years [from 2016 to the current] should the charts be plotted. The default is from 2018 to 2020.",
                                 br(), strong("Select Technical Indicator: "), "Choose which Technical Indicator should be added to the plot. The default is Bollinger Bands.", 
                                 br(), strong("Enter Prediction Date: "), "Choose the date for which you want to forecast the price of the currency",
                                 br(), strong("Select Price to be analysed:"), "Choose whether to analyse the Open or Close Price of the currency. The default is Open",
                                 br(), "The charts are in the 'Charts' tab, while the forecasting is in the 'Forecasts' tab",
                                 br(), "For more information, please see the accompanying website.", 
                                 br(), "This Project has been made by Ayushmaan Dev Verma and Dipanshu Sharma, fourth year Undergraduate Students from the Department of Mathematics, IIT Bombay",
                                 br(), "An accompanying python application has also been made, which is available on the website"),
                        tabPanel("Charts", br(), "The selected cryptocurrency is: ", strong(textOutput("symbol")),
                                 br(), "The Selected Time Range is from Year ", textOutput("year1"), " to Year ", textOutput("year2"),
                                 br(), "The Selected Technical Indicator is: ", textOutput("ta"),
                                 br(), "The price is to be charted is: ", textOutput("pr"),
                                 br(), plotOutput("chart1"),
                                 br(), plotOutput("chart2")),
                        tabPanel("Forecasts",
                                 br(), "The Forecast Date in YYYY/MM/DD is: ", textOutput("dt"),
                                 br(), "The price is to be charted is: ", textOutput("pr1"),
                                 br(), "The Forecast Price is: ", textOutput("fp"),
                                 br(), "The Forecasts are: ", tableOutput("tbl"),
                                 br(), plotOutput("chart3"))
            )
        )
    )
))