library(shiny)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(RColorBrewer)
library(tseries)
library(lubridate)
library(Quandl)
library(forecast)
library(ggplot2)
library(plotly)
Quandl.api_key("zrcB2Ejv9UmvhPCUsy2_")
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

shinyServer(function(input, output){
    output$symbol <- renderText(input$name)
    output$year1 <- renderText(input$year[1])
    output$year2 <- renderText(input$year[2])
    output$date1 <- renderText(as.character(input$daterange[1]))
    output$date2 <- renderText(as.character(input$daterange[2]))
    output$ta <- renderText(input$tech_ind)
    output$dt <- renderText(as.character(input$date))
    output$pr <- renderText(input$price)
    output$pr1 <- renderText(input$price)
    url <- a("repository", href="https://github.com/World-of-Python-and-R/Cryptocurrency-Analysis-and-Forecasting-")
    output$tab1 <- renderUI({
        tagList(url)
    })
    output$tab11 <- renderUI({
        tagList(url)
    })
    
    output$chart1 <- renderPlot({
        input$Button1
        
        symbol <- as.character(isolate(input$name))
        ta <- isolate(input$tech_ind)
        #start.date <- as.Date.yearmon(isolate(input$year[1]))
        #end.date <- as.Date.yearmon(isolate(input$year[2]))
        start.date <- isolate(input$daterange[1])
        end.date <- isolate(input$daterange[2])
        feature <- as.character(isolate(input$price))
        
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
        input$Button1
        
        symbol <- isolate(input$name)
        #start.date <- as.Date.yearmon(isolate(input$year[1]))
        #end.date <- as.Date.yearmon(isolate(input$year[2]))
        start.date <- isolate(input$daterange[1])
        end.date <- isolate(input$daterange[2])
        
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
        input$Button2
        
        symbol <- isolate(input$name)
        forecast_date <- isolate(input$date)
        feature <- as.character(isolate(input$price))
        
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
        input$Button2
        
        forecasts = isolate(predictions())
        price_forecast_mean <- array(forecasts$mean)
        isolate(price_forecast_mean[length(price_forecast_mean)])
    })
    
    output$chart3 <- renderPlot({
        input$Button2
        
        symbol <- isolate(input$name)
        forecast_date <- isolate(input$date)
        feature <- as.character(isolate(input$price))
        
        list_name <- vector(mode = 'list', length = 10)
        arr_symbol <- c("BTC-USD", "ETH-USD", "USDT-USD", "BNB-USD", "ADA-USD", "HEX-USD", "XRP-USD", "USDC-USD", "DOGE-USD", "DOT1-USD") 
        names(list_name) <- c("Bitcoin", "Etherium", "Tether", "Binance", "Cardano", "HEX Coin", "XRP Coin", "USDC Coin", "Dogecoin", "Polka Dot Coin")
        for (i in 1:10) {
            list_name[[i]] <- arr_symbol[i]
        }
        
        data_a <- getSymbols(as.character(list_name[symbol]), auto.assign = F)
        if(feature=="Open"){data_b <- Op(data_a)}
        if(feature=="Close"){data_b <- Cl(data_a)}
        
        forecasts = isolate(predictions())
        means <- xts(forecasts$mean, order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        lower <- xts(forecasts$lower[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        upper <- xts(forecasts$upper[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        total <- rbind.xts(data_b, means)
        total <- merge(total, lower, upper)
        plot(tail(total, 365), main = "Forecasts")
    })
    
    output$tbl <- renderTable({
        input$Button2
        
        forecast_date <- isolate(input$date)
        
        forecasts = isolate(predictions())
        means <- xts(forecasts$mean, order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        lower <- xts(forecasts$lower[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        upper <- xts(forecasts$upper[, 1], order.by = Sys.Date() + 1:julian(forecast_date, Sys.Date()))
        total <- merge( means, lower, upper)
        dates <- as.character(index(total))
        total <- as.data.frame(total)
        total$date <- dates
        names(total) <- c("Mean of Forecasts", "Lower Bound", "Upper Bound", "Date")
        print(total)
    })
})