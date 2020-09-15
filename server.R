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
    isolate(list(r1=input$r1, r2=input$r2, a12=input$a12, a21=input$a21, K=1000))
  })
  
  # Run model and produce animation
  observeEvent(input$go, {
    numSolve <- LVSolve(c(N1=1, N2=1), 20, pList())
    numSolveLong <- pivot_longer(numSolve, cols = c(N1, N2), names_to = "sp", values_to = "N")
  })
  
  output$test <- renderText(pList()$r1)
  
  output$NvtPlot <- renderImage({
    outfile <- tempfile(tmpdir = "img", fileext = ".gif")
    
    p <- ggplot(
      Nvt,
      aes(x = time, y = N, group = sp, colour = factor(sp))
    ) +
      geom_line(show.legend = FALSE, alpha = 0.7) +
      scale_colour_brewer() +
      labs(x = "Time", y = "Number of individuals")+
      geom_point()+
      transition_reveal(time)
    
    anim_save(filename = outfile, animation = animate(p))
    
    list(src = outfile,
         contentType = "image/gif")
  }, deleteFile = TRUE)
  
}


