library(shiny)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(RColorBrewer)
library(tseries)
library(lubridate)
library(Quandl)
library(forecast)
Quandl.api_key("zrcB2Ejv9UmvhPCUsy2_")
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

shinyServer(function(input, output){
    output$symbol <- renderText(input$name)
    output$year1 <- renderText(input$year[1])
    output$year2 <- renderText(input$year[2])
    output$ta <- renderText(input$tech_ind)
    output$dt <- renderText(as.character(input$date))
    output$pr <- renderText(input$price)
    output$pr1 <- renderText(input$price)
    
    output$chart1 <- renderPlot({
        if (input$goButton1 == 0)
            return()
        
        symbol <- as.character(input$name)
        ta <- input$tech_ind
        start.date <- as.Date.yearmon(input$year[1])
        end.date <- as.Date.yearmon(input$year[2])
        feature <- as.character(input$price)
        
        list_name <- vector(mode = 'list', length = 10)
        arr_symbol <- c("BTC-USD", "ETH-USD", "USDT-USD", "BNB-USD", "ADA-USD", "HEX-USD", "XRP-USD", "USDC-USD", "DOGE-USD", "DOT1-USD") 
        names(list_name) <- c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin")
        for (i in 1:10) {
            list_name[[i]] <- arr_symbol[i]
        }
        
        data_a <- getSymbols(as.character(list_name[symbol]), auto.assign = F)
        data_b <- data_a[paste(start.date,end.date,sep="::")]
        if(feature=="Open"){data_b <- Op(data_b)}
        if(feature=="Close"){data_b <- Cl(data_b)}
        if(ta == "Bollinger Bands"){chartSeries(data_b, type = "line", TA = c(addBBands(n=20,sd=2)), theme = chartTheme('white'), name = "Bollinger Bands")}
        if(ta == "Exponential Moving Averages"){chartSeries(data_b, type = "line", TA = c(addEMA(n=30)), theme = chartTheme('white'), name = "Exponential Moving Averages")}
    })
    
    output$chart2 <- renderPlot({
        if (input$goButton1 == 0)
            return()
        
        symbol <- input$name
        start.date <- as.Date.yearmon(input$year[1])
        end.date <- as.Date.yearmon(input$year[2])
        
        list_name <- vector(mode = 'list', length = 10)
        arr_symbol <- c("BTC-USD", "ETH-USD", "USDT-USD", "BNB-USD", "ADA-USD", "HEX-USD", "XRP-USD", "USDC-USD", "DOGE-USD", "DOT1-USD") 
        names(list_name) <- c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin")
        for (i in 1:10) {
            list_name[[i]] <- arr_symbol[i]
        }
        
        data_a <- getSymbols(as.character(list_name[symbol]), auto.assign = F)
        data_b <- data_a[paste(start.date,end.date,sep="::")]
        data_b_DailyReturns <- dailyReturn(Ad(data_b))
        charts.PerformanceSummary(data_b_DailyReturns, main = "Cumulative and Daily Returns, and Drawdown")
    })
    
    predictions <- reactive({
        symbol <- input$name
        forecast_date <- input$date
        feature <- as.character(input$price)
        
        list_name <- vector(mode = 'list', length = 10)
        arr_symbol <- c("BTC-USD", "ETH-USD", "USDT-USD", "BNB-USD", "ADA-USD", "HEX-USD", "XRP-USD", "USDC-USD", "DOGE-USD", "DOT1-USD") 
        names(list_name) <- c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin")
        for (i in 1:10) {
            list_name[[i]] <- arr_symbol[i]
        }
        
        data_a <- getSymbols(as.character(list_name[symbol]), auto.assign = F)
        if(feature=="Open"){data_b <- Op(data_a)}
        if(feature=="Close"){data_b <- Cl(data_a)}
        set.seed(123)
        model <- auto.arima(data_b, lambda = "auto")
        price_forecast <- forecast(model, h = julian(forecast_date, Sys.Date()))
        price_forecast
    })
    
    output$fp <- renderText({
        if (input$goButton2 == 0)
            return()
        forecasts = predictions()
        price_forecast_mean <- array(forecasts$mean)
        price_forecast_mean[length(price_forecast_mean)]
    })
    
    output$chart3 <- renderPlot({
        if (input$goButton2 == 0)
            return()
        symbol <- input$name
        forecast_date <- input$date
        feature <- as.character(input$price)
        
        list_name <- vector(mode = 'list', length = 10)
        arr_symbol <- c("BTC-USD", "ETH-USD", "USDT-USD", "BNB-USD", "ADA-USD", "HEX-USD", "XRP-USD", "USDC-USD", "DOGE-USD", "DOT1-USD") 
        names(list_name) <- c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin")
        for (i in 1:10) {
            list_name[[i]] <- arr_symbol[i]
        }
        
        data_a <- getSymbols(as.character(list_name[symbol]), auto.assign = F)
        if(feature=="Open"){data_b <- Op(data_a)}
        if(feature=="Close"){data_b <- Cl(data_a)}
        
        forecasts = predictions()
        means <- xts(forecasts$mean, order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        lower <- xts(forecasts$lower[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        upper <- xts(forecasts$upper[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        total <- rbind.xts(data_b, means)
        total <- merge(total, lower, upper)
        plot(tail(total, 365), main = "Forecasts")
    })
    
    output$tbl <- renderTable({
        if (input$goButton2 == 0)
            return()
        forecast_date <- input$date
        
        forecasts = predictions()
        means <- xts(forecasts$mean, order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        lower <- xts(forecasts$lower[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        upper <- xts(forecasts$upper[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        total <- merge( means, lower, upper)
        dates <- as.character(index(total))
        total <- as.data.frame(total)
        total$date <- dates
        print(total)
    })
})