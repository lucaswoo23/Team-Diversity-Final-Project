library("dplyr")
library("data.table")
library("ggplot2")
library("TTR")
library("quantmod")
library("Rblpapi")  

library("httr")
library("jsonlite")
library("dplyr")
library("data.table")

# Quandl package must be installed
library(Quandl)

# Get your API key from quandl.com
quandl_api = "LyjxCY3XxHfkd29FAFJy"

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)


google_stocks <- function(sym, start, end) {
  base.url <- paste0('https://www.quandl.com/api/v3/datasets/WIKI/', sym, '/data.json')
  query.params <- list(start_date = start, end_date = end, api_key = quandl_api)
  response <- GET(base.url, query = query.params)
  body <- content(response, "text")
  result <- fromJSON(body)
  cols <- c(1, 9, 10, 11, 12, 13)
  data <- result$dataset_data$data[ , cols]
  colnames(data) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  return(data)
}

test_data <- google_stocks("AAPL", "2016-01-01", "2017-01-01")


aapl_data <- google_stocks("AAPL", "2015-01-01")

# Fetches the data for an individual stock
TSLA_data <- google_stocks('TSLA', "2015-01-01")
ggplot(ROKU_data, aes(ROKU_data$High, ROKU_data$Close)) + geom_point()

SP500_ETF_data <- google_stocks("SPY")    # S&P500 ETF Fund

# Fetches the S&P500 Data from a certain year to a certain year
sp500 <- new.env()
sp500.data <- function(start.date, end.date) {
  start.date <- "2014-01-01"
  end.date <- Sys.Date()
  getSymbols("^GSPC", src = "yahoo", from=start.date, to=end.date)
  SPC <- GSPC$GSPC.Close  # plot(SPC)
}

sp500.2014.data <- sp500.data("2014-01-01", Sys.Date())

# Fetches the NASDAQ Data from a certain year to a certain year
nasdaq<- new.env()
nasdaq.data <- function(start.date, end.date) { 
  getSymbols("^NDX", env = nasdaq, src = "yahoo", from=start.date, to=end.date)
  ndx <- nasdaq$NDX
  NDX <- ndx$NDX.Close
}

nasdaq.2014.data <- nasdaq.data("2014-01-01", Sys.Date())

# Fetches the Dow Jones Data from a certain year to a certain year
dowjones<- new.env()
dow.jones.data <- function(start.date, end.date) {
  getSymbols("^DJI", env = dowjones, src = "yahoo", from=start.date, to=end.date)
  djia <- dowjones$DJI
  DJIA <- djia$DJI.Close
}

dow.Jones.2014.data <- dow.jones.data("2014-01-01", Sys.Date())

# Fetches the Ticker Symbol of a given stock given the name of the company
listings <- stockSymbols()
get.stock.ticker <- function(stock.name) {
  stock.ticker <- listings %>% filter(grepl(stock.name, listings$Name)) %>% select(Symbol)
}

Apple.Ticker <- get.stock.ticker("Apple Inc.")
JohnsonJohnson.Ticker <- get.stock.ticker("Johnson & Johnson")
