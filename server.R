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
  uTime <- 20 # upper time limit to run 
  n <- reactiveValues() # place to put numerical outputs
  
  # isolated reactive for parameter list
  pList <- reactive({
    list(r1=input$r1, r2=input$r2, a12=input$a12, a21=input$a21, K=1000)
  })
  
  # Update on hitting Go
  observeEvent(input$go, {
    n$short <- LVSolve(N0 = c(N1=1, N2=1), maxTime = uTime, pars = pList())
    n$long <- pivot_longer(n$short, cols = c(N1, N2), names_to = "sp", values_to = "N")
  })
  
  output$test <- renderText(pList()$r1)
  
  # Make image gif for N v t
  output$NvtPlot <- renderImage({
    p <- ggplot(
      n$long,
      aes(x = time, y = N, group = sp, colour = factor(sp))
    ) +
      geom_line(show.legend = FALSE, alpha = 0.7) +
      scale_colour_brewer() +
      labs(x = "Time", y = "Number of individuals")+
      geom_point()+
      transition_reveal(time)
    
    outfile <- tempfile(tmpdir = "img", fileext = ".gif")
    anim_save(filename = outfile, animation = animate(p, nframes = uTime, fps = uTime/5))
    
    list(src = outfile,
         contentType = "image/gif")
  }, deleteFile = TRUE)
  
  # Make image gif for N1 v N2
  output$NvNPlot <- renderImage({
    p <- ggplot(
      n$short,
      aes(x = N2, y = N1)
    ) +
      geom_line(show.legend = FALSE, alpha = 0.7) +
      scale_colour_brewer() +
      labs(x = "Number of species 2", y = "Number of species 1")+
      geom_point()+
      transition_reveal(time)
    
    outfile <- tempfile(tmpdir = "img", fileext = ".gif")
    anim_save(filename = outfile, animation = animate(p, nframes = uTime, fps = uTime/5))
    
    list(src = outfile,
         contentType = "image/gif")
  }, deleteFile = TRUE)
  
}


