#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Analysis"),
  
  # Sidebar with a slider input for number of bins 
  
  tabsetPanel( 
    tabPanel(
      "Individual Stock",
      sidebarLayout(
         sidebarPanel(
           
            textInput("Name", label = h3("Stock Ticker:"), value = "TSLA"),
            
            dateRangeInput("Date", label = h3("Date:"), start = "2017-11-29", end = Sys.Date()),
            
            hr(),
            
            textInput("text", label = h5("Enter Name of Company to Receive Ticker Information"),
                      value = "Enter Company Name")
            
         ),
         mainPanel(
           plotOutput("distPlot"),
           hr(),
           dataTableOutput("asdf")
         )
      )
    ),
    
    tabPanel(
      "Overview",
       sidebarLayout(
          sidebarPanel(
            radioButtons("radio", label = h3("Select Timeline"),
                         choices = list("5 Day" = 1, "1 Month" = 2, "6 Month" = 3,
                                        "1 Year" = 4, "5 Year" = 5, "Max" = 6),
                         selected = 3)
          ),
          mainPanel(
            plotOutput("sp500"),
            plotOutput("dow_jones"),
            plotOutput("nasdaq")
          )
      )
    )
  )

))
