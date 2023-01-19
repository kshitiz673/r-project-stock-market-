library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)
#dataset
#collection of the dataset
tickers <- c("TSLA","AMZN","ADDYY","AAPL","TTM","NKE","PBYI")
benchmarks <- c("^NDX","^GSPC")

prices <- tq_get(tickers, 
                 get  = "stock.prices",
                 from = today()-months(12),
                 to   = today(),
                 complete_cases = F) %>%select(symbol,date,close)

bench <- tq_get(benchmarks,
                get  = "stock.prices",
                from = today()-months(12),
                to   = today()) %>%
  select(symbol,date,close)

# -----------------------------------------------------
# UI
#-------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cyborg"),
                
                # Titlepanel
                titlePanel("Stock Market Analysis"),
                
                # Sidebar 
                sidebarLayout(
                  sidebarPanel(width = 3,
                               
                               # Let user pick stocks
                               pickerInput(
                                 inputId =  "stocks",
                                 label = h5("STOCKS"),
                                 choices = c(
                                   "TESLA"      = tickers[1], 
                                   "AMAZON"     = tickers[2],
                                   "ADIDAS"     = tickers[3],
                                   "APPLE"      = tickers[4],
                                   "TATA MOTORS"= tickers[5],
                                   "NIKE"       = tickers[6],
                                   "PUMA"       = tickers[7]),
                                 selected = tickers,   
                                 options = list(`actions-box` = TRUE), 
                                 multiple = T
                               ),
                               
                               # Pick time period
                               radioButtons("period", label = h4("Period"),
                                            choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
                                            selected = 4
                               ),
                               
                               # Pick benchmark
                               radioButtons("benchmark", label = h4("Benchmark"),
                                            choices = list("SP500" = 1, "Nasdaq100" = 2,"None" = 3),
                                            selected = 3)
                  ),
                  
                  # Plot results
                  mainPanel(
                    plotlyOutput("plot",height=400)
                  )
                )
)

# -----------------------------------------------------
# SERVER
#-------------------------------------------------------

server <- function(input, output) {
  
  # server logic based on user input
  observeEvent(c(input$period,input$stocks,input$benchmark), {
    
    prices <- prices %>%
      filter(symbol %in% input$stocks)
    
    if (input$period == 1) {
      prices <- prices %>%
        filter(
          date >= today()-months(1)) }
    
    if (input$period == 2) {
      prices <- prices %>%
        filter(date >= today()-months(3)) }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    
    if (input$period == 5) {
      prices <- prices %>%
        filter(year(date) == year(today())) }
    
    if (input$benchmark == 1) {
      bench <- bench %>%
        filter(symbol=="^GSPC",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 2) {
      bench <- bench %>%
        filter(symbol=="^NDX",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    # Create plot
    output$plot <- renderPlotly({
      print(
        ggplotly(prices %>%
                   group_by(symbol) %>%
                   mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                   mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                   ungroup() %>%
                   ggplot(aes(date, value,colour = symbol)) +
                   geom_line(size = 1, alpha = .9)+
                   # uncomment the line below to show area under curves
                   #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                   theme_minimal(base_size=16) +
                   theme(axis.title=element_blank(),
                         plot.background = element_rect(fill = "black"),
                         panel.background = element_rect(fill="black"),
                         # panel.grid = element_blank())
                         panel.grid.major = element_line(color = "red", size = 0.5),
                         panel.grid.minor = element_line(color = "red",size=0.5),
                         legend.text = element_text(colour="white"))
                 
        )
      )
    })
  })
}

# Run the application 
shinyApp(ui,server)


