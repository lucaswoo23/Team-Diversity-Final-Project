library("dplyr")
library("data.table")
library("ggplot2")
library("TTR")
library("quantmod")
#library("Rblpapi")  
# Quandl package must be installed
library(Quandl)

# Get your API key from quandl.com
quandl_api = "LyjxCY3XxHfkd29FAFJy"

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)

google_stocks <- function(sym, start_date, end_date) {
  require(devtools)
  require(Quandl)
  # create a vector with all lines
  google_out = tryCatch(Quandl(c(
    paste0("WIKI/", sym, ".8"),  #  Adj. Open
    paste0("WIKI/", sym, ".9"),  # Adj. High
    paste0("WIKI/", sym, ".10"), # Adj. Low
    paste0("WIKI/", sym, ".11"), # Adj. Close
    paste0("WIKI/", sym, ".12")), # Adj. Volume
    start_date = start_date,
    type = "zoo"
  ))
  start_date <- as.Date(start_date)
  end_date <-as.Date(end_date)
  google_out <- as.data.frame(google_out) %>% 
    tibble::rownames_to_column()
  names(google_out) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  #a <- google_out$Date
  #a <- as.Date(a)
  google_out <- filter(google_out, start_date <= as.Date(Date, format = "%Y-%m-%d") & end_date >= as.Date(Date, format = "%Y-%m-%d"))
  return(google_out)
}

# aapl_data <- google_stocks("AAPL", "2016-01-01")

# Fetches the data for an individual stock
TSLA_data <- google_stocks("AAPL", "2017-01-01", "2017-01-20")
TSLA_data$Date <- as.Date(TSLA_data$Date, format = "%Y-%m-%d")
View(TSLA_data)

ggplot(TSLA_data, aes(Date, Close, group = 1)) +
  geom_point(aes(color = Volume)) +
  geom_line() 



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
get.stock.ticket("Apple")

Apple.Ticker <- get.stock.ticker("Apple Inc.")
JohnsonJohnson.Ticker <- get.stock.ticker("Johnson & Johnson")

