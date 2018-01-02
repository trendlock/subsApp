

# on CRAN
library(shiny)
library(tidyverse)
library(lubridate)
library(glue)
library(plotly)

library(shinyWidgets)

library(shinysense)




# Define UI for application that draws a histogram
shinyUI(
  fluidPage(titlePanel("Draw Eff Curves"),
            sidebarLayout(
              
              sidebarPanel(
                
                
                downloadButton("csv_curves"),
                
                numericInput("num1", label = "Poly Deg 1", value = 3),
                numericInput("num2", label = "Poly Deg 2", value = 3),
                numericInput("num3", label = "Poly Deg 3", value = 3),
                numericInput("num4", label = "Poly Deg 4", value = 3),
                numericInput("num5", label = "Poly Deg 5", value = 3),
                numericInput("num6", label = "Poly Deg 6", value = 3),
                
                radioGroupButtons(inputId = "tog_input", 
                                  choices = c("Input Jet 1",  
                                              "Input Jet 2",  
                                              "Input Jet 3",  
                                              "Input Propeller 1",  
                                              "Input Propeller 2",  
                                              "Input Propeller 3"), 
                                  selected = "Input Jet 1")
                
              ),
              mainPanel(
                plotlyOutput("eff_plot"),
                shinydrawrUI("drawr_plot")
              )
              
            )))


