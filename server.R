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
server <- function(input, output, session) {

  n <- reactiveValues() # place to put numerical outputs
  
  # isolated reactive for parameter list
  pList <- reactive({
    list(r1=input$r1, r2=input$r2, a12=input$a12, a21=input$a21, K=1000)
  })
  

  # Update on hitting Go
  observeEvent(input$go, {
    n$short <- LVSolve(N0 = c(N1=input$N1, N2=input$N2), maxTime = input$gens, pars = pList())
    n$long <- pivot_longer(n$short, cols = c(N1, N2), names_to = "sp", values_to = "N")
  }, ignoreNULL = FALSE)
  
  #output$test <- renderText(session$clientData$output_myImage_width)
  
  # Make image gif for N v t
  output$NvtPlot <- renderImage({
    info <- getCurrentOutputInfo()
    if (input$anim == "Yes"){
      p <- ggplot(
        n$long,
        aes(x = time, y = N, group = sp, color = factor(sp))
      ) +
        scale_color_discrete(name="Species",
                             breaks=c("N1", "N2"),
                             labels=c("1", "2")) +
        geom_path(alpha = 0.7, size = 0.9) +
        theme(legend.position="top") +
        labs(x = "Time", y = "Number of individuals")+
        geom_point()+
        transition_reveal(time)
      outfile <- tempfile(tmpdir = "img", fileext = ".gif")
      nf <- min(input$gens, 100)
      anim_save(filename = outfile, animation = animate(p, nframes = nf, fps = nf/5, width = info$width(),
                                                        height = info$height(), units = "px"))
    } else {
      p <- ggplot(
        n$long,
        aes(x = time, y = N, group = sp, color = factor(sp))
      ) +
        geom_path(alpha = 0.7, size = 0.9) +
        scale_color_discrete(name="Species",
                            breaks=c("N1", "N2"),
                            labels=c("1", "2")) +
        theme(legend.position="top") +
        labs(x = "Time", y = "Number of individuals")
      outfile <- tempfile(tmpdir = "img", fileext = ".png")
      ggsave(filename = outfile, plot = p, width = info$width()/36,
             height = info$height()/36, units = "cm")
    }
    
    list(src = outfile,
         contentType = "image/gif",
         width = info$width(),
         height = info$height())
  }, deleteFile = TRUE)
  
  # Make image gif for N1 v N2
  output$NvNPlot <- renderImage({
    info <- getCurrentOutputInfo()
    if (input$anim == "Yes"){
      p <- ggplot(
        n$short,
        aes(x = N1, y = N2)
      ) +
        geom_path(alpha = 0.7, size = 0.9) +
        labs(x = "Number of species 1", y = "Number of species 2")+
        geom_point()+
        transition_reveal(time)
      
      outfile <- tempfile(tmpdir = "img", fileext = ".gif")
      nf <- min(input$gens, 100)
      anim_save(filename = outfile, animation = animate(p, nframes = nf, fps = nf/5, width = info$width(),
                                                        height = info$height(), units = "px"))
    } else {
      p <- ggplot(
        n$short,
        aes(x = N1, y = N2)
      ) +
        geom_path(alpha = 0.7, size = 0.9, arrow = arrow()) +
        geom_point()+
        labs(x = "Number of species 1", y = "Number of species 2")
      outfile <- tempfile(tmpdir = "img", fileext = ".png")
      ggsave(filename = outfile, plot = p, width = info$width()/36,
             height = info$height()/36, units = "cm")
    }
    
    list(src = outfile,
         contentType = "image/gif",
         width = info$width(),
         height = info$height())
  }, deleteFile = TRUE)
  
}


