library("shiny")
library("dplyr")
library("data.table")
library("ggplot2")
library("TTR")
library("quantmod")
library("Quandl")

# Get your API key from quandl.com
quandl_api = "LyjxCY3XxHfkd29FAFJy"

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Fetches the Individual Stock's Information Given the Ticker Symbol, the Starting Date, and the Ending Date
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
    end_date <- as.Date(end_date)
    google_out <- as.data.frame(google_out) %>% 
                  tibble::rownames_to_column()
    names(google_out) <- c("Date", "Open", "High", "Low", "Close", "Volume")

    google_out <- filter(google_out, start_date <= as.Date(Date, format = "%Y-%m-%d") & end_date >= as.Date(Date, format = "%Y-%m-%d"))
    return(google_out)
  }
  
  # Grabs the start date and end date that the user wishes to use
  get.information <- function() {
    date_vector <- input$Date 
    start_date <- date_vector[1]
    end_date <- date_vector[2]
    chosen_stock_info <- google_stocks(input$Name, start_date, end_date)
  }
  
  # Outputs the minimum stock price and date of an individual stock
  output$min <- renderText({
     chosen_stock_info <- get.information()
     min <- filter(chosen_stock_info, Close == min(Close))
     min.close <- min$Close
     min.date <- min$Date
     paste0("The minimum was ", min.close, " on ", min.date)
  })
  
  # Outputs the maxium stock price and date of an individual stock
  output$max <- renderText({
     chosen_stock_info <- get.information()
     max <- filter(chosen_stock_info, Close == max(Close))
     max.close <- max$Close
     max.date <- max$Date
     paste0("The maximum was ", max.close, " on ", max.date) 
  })
  
  # Plots the Individual Stock Information
  output$distPlot <- renderPlot({
    chosen_stock_info <- get.information()
    chosen_stock_info$Date <- as.Date(chosen_stock_info$Date, format = "%Y-%m-%d")
    ggplot(chosen_stock_info, aes(x = Date, y = Close, group = 1)) +
      geom_point(aes(color = Volume)) +
      geom_line() +
      ggtitle(paste0(input$Name,  " Information"))
  })
  
  # Fetches the Stock Information (Name, Ticker, Industry, etc.)
  listings <- stockSymbols()
  get.stock.ticker <- function(stock.name) {
    stock.ticker <- listings %>% filter(grepl(stock.name, listings$Name)) %>% select(Symbol, Name)
    return(stock.ticker)
  }
  
  # Outputs the text of the stock ticker
  output$stock.df <- renderDataTable({
    stock.ticker <- get.stock.ticker(input$text)
  })
  
  # Fetches the S&P 500 Data from a certain date to current date
  sp500 <- new.env()
  sp500.data <- function(start.date) {
    df <- getSymbols("^GSPC", src = "yahoo", from=start.date, to=Sys.Date())
    SPC <- data.frame(GSPC$GSPC.Close, GSPC$GSPC.Volume)
    SPC <- as.data.frame(SPC) %>% 
           tibble::rownames_to_column()
    colnames(SPC) <- c("Date", "Close", "Volume")
    SPC$Date <- as.Date(SPC$Date, format = "%Y-%m-%d")
    return(SPC)
  }
  
   # Fetches the Dow Jones Data from a certain date to current date
  dowjones<- new.env()
  dow.jones.data <- function(start.date) {
     df <- getSymbols("^DJI", env = dowjones, src = "yahoo", from=start.date, to=Sys.Date())
     djia <- dowjones$DJI
     DJIA <- data.frame(djia$DJI.Close, djia$DJI.Volume)
     DJIA <- as.data.frame(DJIA) %>%
             tibble::rownames_to_column()
     colnames(DJIA) <- c("Date", "Close", "Volume")
     DJIA$Date <- as.Date(DJIA$Date, format = "%Y-%m-%d")
     return(DJIA)
  }
  
  # Fetches the NASDAQ Data from a certain date to current date
  nasdaq <- new.env()
  nasdaq.data <- function(start.date) { 
     df <- getSymbols("^NDX", env = nasdaq, src = "yahoo", from=start.date, to=Sys.Date())
     ndx <- nasdaq$NDX
     NDX <- data.frame(ndx$NDX.Close, ndx$NDX.Volume)
     NDX <- as.data.frame(NDX) %>%
            tibble::rownames_to_column()
     colnames(NDX) <- c("Date", "Close", "Volume")
     NDX$Date <- as.Date(NDX$Date, format = "%Y-%m-%d")
     return(NDX)
  }
  
  find.date <- function() {
    date <- 0
    if(input$radio == 1) {
      date <- 7
    }else if(input$radio == 2) {
      date <- 30
    }else if(input$radio == 3) {
      date <- 188
    }else if(input$radio == 4) {
      date <- 365
    }else if(input$radio == 5) {
      date <- 365 * 5
    }else {
      date <- 365 * 100
    }
    return(date)
  }
  
  # Plots the S&P 500 Information
  output$sp500 <- renderPlot({
     date <- find.date()
     sp.data <- sp500.data(Sys.Date() - date)
     ggplot(sp.data, aes(x = Date, y = Close, group = 1)) +
           geom_point(aes(color = Volume)) +
           geom_line() +
           ggtitle("S&P 500 Data")
  })
  
  # Plots the Dow Jones Information 
  output$dow_jones <- renderPlot({
     date <- find.date()
     dj.data <- dow.jones.data(Sys.Date() - date)
     ggplot(dj.data, aes(x = Date, y = Close, group = 1)) +
           geom_point(aes(color = Volume)) +
           geom_line() +
           ggtitle("Dow Jones Industrial Average Data")
  })
  
  # Plots the NASDAQ Information
  output$nasdaq <- renderPlot({
     date <- find.date()
     nasdaq.data <- nasdaq.data(Sys.Date() - date)
     ggplot(nasdaq.data, aes(x = Date, y = Close, group = 1)) +
            geom_point(aes(color = Volume)) +
            geom_line() +
            ggtitle("NASDAQ-100 Market Index Data")
  })
  
  output$comparison.plot <- renderPlot({
     
     filtered_sector <- filter(listings, Sector == 'Basic Industries')
     top_five <- head(arrange(filtered_sector,desc(MarketCap)), n = 5)
     empty_dataframe <- data.frame(matrix(ncol = 4, nrow = 0))
     values <- c("Date", "Close", "Volume", "Symbol")
     colnames(empty_dataframe) <- values
     empty_dataframe$Date <- as.Date(empty_dataframe$Date)
     
     for (i in nrow(top_five)) {
        symbol <- top_five[i,1]
        new_df <- google_stocks(symbol, "2017-09-01", Sys.Date())
        selected_df <- select(new_df, Date, Close, Volume) 
        selected_df$Date <- as.Date(selected_df$Date)
        empty_dataframe <- full_join(empty_dataframe, selected_df)
        empty_dataframe$Symbol <- symbol
     }
     
     ggplot(data=empty_dataframe, aes(x=Date, y=Close, group = Symbol, colour = Volume)) +
        geom_line() +
        geom_point()
  })
  
})
