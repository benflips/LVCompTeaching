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
library(ggplot2)
library(gifski)
library(gganimate)
## ---------------------------

## load up our functions into memory
source("LVFunctions.R")
## ---------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # isolated reactive for parameter list
  pList <- reactive({
    list(r1=input$r1, r2=input$r2, a12=input$a12, a21=input$a21, K=1000)
  })
  
  output$test <- renderText(pList()$r1)
  
}


