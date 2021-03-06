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
            p("Set parameter values, and hit go."),
            sliderInput("N1",
                        "Initial number of species 1:",
                        min = 0,
                        max = 10,
                        value = 1,
                        step = 1),
            sliderInput("N2",
                        "Initial number of species 2:",
                        min = 0,
                        max = 10,
                        value = 1,
                        step = 1),
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
            actionButton("go",
                         "Go!"),
            p(div(HTML("Site code is available <a href = https://github.com/benflips/LVCompTeaching>here</a>.")))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # textOutput("test"),
            h3("Numbers over time: both species"),
            plotlyOutput("NvtPlot"),
            p(),
            h3("The phase plane: N2 vs N1"),
            p("Red trace shows the trajectory of the system over time."),
            plotlyOutput("NvNPlot"),
            h4("Outcome"),
            textOutput("outcome"),
            tableOutput("pointTableN1"),
            tableOutput("pointTableN2")
        )
    )
)

