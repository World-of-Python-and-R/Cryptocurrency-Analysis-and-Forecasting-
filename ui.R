library(shiny)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(RColorBrewer)
library(tseries)
library(lubridate)
library(Quandl)
library(ggplot2)
library(plotly)
library(reshape2)
Quandl.api_key("zrcB2Ejv9UmvhPCUsy2_")
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

shinyUI(fluidPage(
    titlePanel("Cryptocurrency Analysis Project"),
    sidebarLayout(
        sidebarPanel(
            selectInput("name", "Select Cryptocurrency:", c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin")),
            #sliderInput("year", "Select Time Range:", min = 2016, max = as.numeric(format(as.Date(Sys.Date()), '%Y'))+1, value = c(2018, 2020), sep = "", pre = "Year "),
            dateRangeInput("daterange", "Select Date Range:", start = "2018-01-01", end = "2020-01-01", min = "2014-01-01", max = Sys.Date(), separator = " & "),
            selectInput("tech_ind", "Select Technical Indicator:", c("Bollinger Bands", 
                                                                      "Exponential Moving Averages")),
            selectInput("price", "Select Price to be analysed:", c("Open", "Close")),
            actionButton("Button1", "Chart!"),
            br(),
            dateInput("date", "Enter the Prediction Date:", value = Sys.Date()+1),
            actionButton("Button2", "Forecast!"),
            br(), br(),
            selectInput("comp", "Select Cryptocurrency:", c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin"), multiple = T, selected = c("Binance", "Etherium")),
            actionButton("Button3", "Compare!")
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Documentation", br(), br(), strong("Select Cryptocurrency: "), "Select a currency from the given list of 10 to chart. The default is Bitcoin.",
                                 #br(), strong("Select Time Range: "), "Choose for which years [from 2016 to the current] should the charts be plotted. The default is from 2018 to 2020.",
                                 br(), 
                                 br(), strong("Select Date Range: "), "Choose for which dates [from 1 Jan 2014 to the current date] should the charts be plotted. The default is from 1 Jan 2018 to 1 Jan 2020 and the format is yyyy/mm/dd.",
                                 br(), 
                                 br(), strong("Select Technical Indicator: "), "Choose which Technical Indicator should be added to the plot. The default is Bollinger Bands.", 
                                 br(), 
                                 br(), strong("Enter Prediction Date: "), "Choose the date for which you want to forecast the price of the currency",
                                 br(), 
                                 br(), strong("Select Price to be analysed:"), "Choose whether to analyse the Open or Close Price of the currency. The default is Open",
                                 br(), 
                                 br(), "The charts are in the 'Charts' tab, while the forecasting is in the 'Forecasts' tab",
                                 br(), 
                                 br(), "For more information, please see the accompanying github", uiOutput("tab1", inline = T),  
                                 br(), 
                                 br(), "This Project has been made by Ayushmaan Dev Verma and Dipanshu Sharma, fourth year Undergraduate Students from the Department of Mathematics, IIT Bombay",
                                 br(), 
                                 br(),"An accompanying python application has also been made, which is available on the", uiOutput("tab11", inline = T)),
                        tabPanel("Charts", br(), br(), "The selected cryptocurrency is: ", strong(textOutput("symbol", inline = T)),
                                 #br(), "The Selected Time Range is from Year ", textOutput("year1"), " to Year ", textOutput("year2"),
                                 br(), 
                                 br(), "The Selected Date Range is from ", strong(textOutput("date1", inline = T)), " to ", strong(textOutput("date2", inline = T)),
                                 br(), 
                                 br(), "The Selected Technical Indicator is: ", strong(textOutput("ta", inline = T)),
                                 br(), 
                                 br(), "The price is to be charted is: ", strong(textOutput("pr", inline = T)),
                                 br(), 
                                 br(), plotOutput("chart1"),
                                 br(), 
                                 br(), plotOutput("chart2")),
                        tabPanel("Comparison", br(), br(), "The selected cryptocurrency are: ", strong(textOutput("comps", inline = T)),
                                 #br(), "The Selected Time Range is from Year ", textOutput("year1"), " to Year ", textOutput("year2"),
                                 br(), 
                                 br(), "The Selected Date Range is from ", strong(textOutput("date11", inline = T)), " to ", strong(textOutput("date12", inline = T)),
                                 br(), 
                                 br(), "The price is to be charted is: ", strong(textOutput("pr1", inline = T)),
                                 br(), 
                                 br(), plotlyOutput("chart3")),
                        tabPanel("Forecasts", br(), br(), "The Forecast Date in YYYY/MM/DD is: ", strong(textOutput("dt", inline = T)),
                                 br(), 
                                 br(), "The price is to be charted is: ", strong(textOutput("pr2", inline = T)),
                                 br(), 
                                 br(), "The Forecast Price is: ", strong(textOutput("fp", inline = T)),
                                 br(), 
                                 br(), "The Forecasts are: ", tableOutput("tbl"),
                                 br(), 
                                 br(), plotOutput("chart4"))
            )
        )
    )
))