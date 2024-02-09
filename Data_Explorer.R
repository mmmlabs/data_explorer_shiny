library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "MMM Labs: Data Explorer"),
  dashboardSidebar(
    fileInput("uploadSalesData", "Upload Sales Data", accept = c(".csv", "text/csv")),
    fileInput("uploadMarketingData", "Upload Marketing Data", accept = c(".csv", "text/csv")),
    actionButton("simulateDataBtn", "Simulate Data")
  ),
  dashboardBody(
    tabsetPanel(id = "tabs", type = "tabs",
                tabPanel("Sales", value = "Sales",
                         div(style = "padding: 20px;", 
                             tags$input(type = "text", class = "form-control", placeholder = "Ask me anything (MMM related)", style = "margin-bottom: 20px;")),
                         DTOutput("salesSummaryTable"),
                         plotOutput("salesTimeSeriesPlot")
                ),
                tabPanel("Marketing", value = "Marketing",
                         div(style = "padding: 20px;", 
                             tags$input(type = "text", class = "form-control", placeholder = "Ask me anything (MMM related)", style = "margin-bottom: 20px;")),
                         DTOutput("marketingSummaryTable"),
                         plotOutput("marketingTimeSeriesPlot")
                ),
                tabPanel("Others", value = "Others", h2("Other Data View")),
                tabPanel("Exploratory", value = "Exploratory", h2("Exploratory Analysis View")),
                tabPanel("Log", value = "Log", h2("Log Information View"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  salesData <- reactiveVal()
  marketingData <- reactiveVal()
  
  observeEvent(input$uploadSalesData, {
    df <- read.csv(input$uploadSalesData$datapath)
    df$Date <- as.Date(df$Date)
    salesData(df)
  })
  
  observeEvent(input$uploadMarketingData, {
    df <- read.csv(input$uploadMarketingData$datapath)
    df$Date <- as.Date(df$Date)
    marketingData(df)
  })
  
  output$salesSummaryTable <- renderDT({
    req(salesData())
    salesData() %>%
      group_by(Sales_Channel) %>%
      summarise(Net_Revenue = sum(Net_Revenue), Units_Sold = sum(Units_Sold)) %>%
      datatable()
  })
  
  output$salesTimeSeriesPlot <- renderPlot({
    req(salesData())
    ggplot(salesData(), aes(x = Date, y = Net_Revenue, group = Sales_Channel, color = Sales_Channel)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Net Revenue by Sales Channel", x = "Date", y = "Net Revenue")
  })
  
  output$marketingSummaryTable <- renderDT({
    req(marketingData())
    marketingData() %>%
      group_by(Marketing_Channel) %>%
      summarise(Ad_Spend = sum(Ad_Spend), Impressions = sum(Impressions), Clicks = sum(Clicks)) %>%
      datatable()
  })
  
  output$marketingTimeSeriesPlot <- renderPlot({
    req(marketingData())
    ggplot(marketingData(), aes(x = Date, y = Ad_Spend, group = Marketing_Channel, color = Marketing_Channel)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Ad Spend by Marketing Channel", x = "Date", y = "Ad Spend")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
