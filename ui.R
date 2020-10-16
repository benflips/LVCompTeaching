## ---------------------------
##
## Script name: ui.R
##
## Purpose of script: User interface for web app to teach Lotka Volterra dynamics
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
library(ggplot2)
library(gifski)
library(gganimate)
library(plotly)
## ---------------------------

## load up our functions into memory

## ---------------------------



# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Lotka-Volterra Competition"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("Set parameter values and then hit, go."),
            sliderInput("N1",
                        "Initial number of species 1:",
                        min = 0,
                        max = 1000,
                        value = 1,
                        step = 50),
            sliderInput("N2",
                        "Initial number of species 2:",
                        min = 0,
                        max = 1000,
                        value = 1,
                        step = 50),
            sliderInput("r1",
                        "Intrinsic growth rate species 1:",
                        min = 0,
                        max = 3,
                        value = 1.0,
                        step = 0.1),
            sliderInput("r2",
                        "Intrinsic growth rate species 2:",
                        min = 0,
                        max = 3,
                        value = 1.1,
                        step = 0.1),
            sliderInput("K1",
                        "Carrying capacity, species 1:",
                        min = 0,
                        max = 1000,
                        value = 500,
                        step = 50),
            sliderInput("K2",
                        "Carrying capacity, species 2:",
                        min = 0,
                        max = 1000,
                        value = 500,
                        step = 50),
            sliderInput("a12",
                        "Exchange rate, 1 from 2:",
                        min = 0,
                        max = 2,
                        value = 1,
                        step = 0.1),
            sliderInput("a21",
                        "Exchange rate, 2 from 1:",
                        min = 0,
                        max = 2,
                        value = 1.1, 
                        step = 0.1),
            sliderInput("gens",
                        "Number of generations:",
                        min = 10,
                        max = 300,
                        value = 40, 
                        step = 10),
            radioButtons("anim", 
                         label = "Animate?", 
                         choices = c("Yes", "No"), 
                         selected = "No", 
                         inline = TRUE),
            p("Note, animation may take quit a while to process and it is best to wait."),
            actionButton("go",
                         "Go!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # textOutput("test"),
            h2("Numbers over time: both species"),
            plotlyOutput("NvtPlot2"),
            p(),
            h2("The phase plane: N1 vs N2"),
            imageOutput("NvNPlot")
        )
    )
)

