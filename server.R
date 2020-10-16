## ---------------------------
##
## Script name: server.R
##
## Purpose of script: server function for web app to teach Lotka-Volterra competition dynamics
##
## Author: Ben Phillips
##
## Date Created: 2020-09-15
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 
library(shiny)
library(tidyr)
library(plotly)
## ---------------------------

## load up our functions into memory
source("LVFunctions.R")
## ---------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  n <- reactiveValues() # place to put numerical outputs

  ##### Parameter list #####  
  # isolated reactive for parameter list
  pList <- reactive({
    list(r1=input$r1, r2=input$r2, a12=input$a12, a21=input$a21, K1=input$K1, K2 = input$K2)
  })
  
  ##### Action on Go ##### 
  # Update on hitting Go
  observeEvent(input$go, {
    n$short <- LVSolve(N0 = c(N1=input$N1, N2=input$N2), maxTime = input$gens, pars = pList())
    n$long <- pivot_longer(n$short, cols = c(N1, N2), names_to = "sp", values_to = "N")
    n$isoPoints <- isoPoints(pList())
  }, ignoreNULL = FALSE)
  
  #output$test <- renderText(n$isoPoints$iso2)

  ##### N v t plot #####  
  output$NvtPlot <- renderPlotly({
    p <- plot_ly(data = n$short, type = "scatter", mode = "none") %>% 
      add_trace(y = ~N1, x = ~time, mode = "lines", name = "N1") %>%
      add_trace(y = ~N2, x = ~time, mode = "lines", name = "N2") %>%
      layout(yaxis = list(title = list(text = "N(t)"),
                          type = "linear"),
             xaxis = list(title = list(text = "Time")),
             legend = list(x = 0)) %>%
      config(displayModeBar = FALSE)
  })
  

  ##### N2 v N1 plot #####  
  output$NvNPlot <- renderPlotly({
    p <- plot_ly(data = n$short, type = "scatter", mode = "none") %>% 
      add_trace(y = ~N2, x = ~N1, mode = "lines", col = "black") %>%
      layout(
        showlegend = FALSE,
        yaxis = list(title = list(text = "N2"),
                          type = "linear"),
             xaxis = list(title = list(text = "N1")),
             legend = list(x = 0)) %>%
      config(displayModeBar = FALSE)
  })
  
}


