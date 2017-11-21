
# Libraries ----
library(shiny)
library(tidyverse)
library(lubridate)

#devtools::install_github("ropensci/plotly")
library(plotly)

#devtools::install_github("Appsilon/shiny.semantic")
library(shiny.semantic)

#devtools::install_github("rstudio/shinythemes")
library(shinythemes)

#devtools::install_github("nstrayer/shinysense")
library(shinysense)



# UI ----
shinyUI(
  navbarPage(
    theme = shinytheme("united"),
    title = "App Title",


    # Tab 1 ====
    tabPanel(
      title = "First Tab Title",

    fluidPage(
      splitLayout(
        #tags$style("body {background-color: #ddd9c5;}"),

        # Main Subs Plot ====
          verticalLayout(

                          plotlyOutput("eff_plot"),
                          shinydrawrUI("drawr_plot")

            ),

          # Assumptions and params ====

          fluidPage(
            semanticPage(

              fluidRow(
                div(class = "ui horizontal divider", uiicon("settings"), "Assumptions"),
                br(),
                column(4,
                       # Hotel Load slider ====
                       div(class = "ui card",
                           div(class = "content",
                               div(class = "right floated meta", "(kW)"),
                               uiicon("settings"),
                               "Hotel Load"),
                           div(class = "content",
                               sliderInput("hotel_load", NULL, 75, 300, 200, post = " kW")),
                           br(), br(), br(), br())),
                column(4,
                       # Onborded Batt  ====
                       div(class = "ui card",
                           div(class = "content",
                               div(class = "right floated meta", "(Tonnes)"),
                               uiicon("settings"),
                               "Batteries"),
                           div(class = "content",
                               sliderInput("onboarded_batt", "(Onboarded)", 100, 700, 500, post = " Tonnes")),
                           br(), br(), br())),
                column(4,
                       # Hotel Load slider ====
                       div(class = "ui card",
                           div(class = "content",
                               div(class = "right floated meta", " "),
                               uiicon("settings"),
                               "Battery Energy Density"),
                           div(class = "content",
                               sliderInput("batt_energy_MJ_kg", NULL, 0.08, 0.8, 0.46, step = 0.02, post = " MJ/kg"),
                               sliderInput("batt_energy_Wh/kg", NULL, 22.2, 222, 124, post = " Wh/kg"))))),
              br(),
              fluidRow(
                div(class = "ui horizontal divider", uiicon("settings"), "Power Reference Point"),
                br(),
                column(4,
                       # Hotel Load Match ====
                       div(class = "ui card",
                           div(class = "content",
                               div(class = "right floated meta", "(kts)"),
                               uiicon("settings"),
                               "Hotel Match"),
                           div(class = "content",
                               actionButton("HL_jet_but", "Jet"),
                               actionButton("HL_prop_but", "Prop"),
                               br(),br(),
                               HTML("<p>Choose speed at which</p>"),
                               HTML("<p>Hotel Load = Power Drawn</p>"),
                               HTML("<p>by propulsion systems:</p>"),
                               br(),
                               sliderInput("HLM_patrol_speed", NULL, 0.5, 7, 5, step = 0.5, post = " kts")),
                           br(), br(), br(), br()
                       )),
                column(4,
                       # Top Speed + Power Match ====
                       div(class = "ui card",
                           div(class = "content",
                               div(class = "right floated meta", " "),
                               uiicon("settings"),
                               "Top Speed + Power"),
                           div(class = "content",
                               actionButton("TSP_jet_but", "Jet"),
                               actionButton("TSP_prop_but", "Prop"),
                               br(), br(),
                               HTML("<p>Choose max power of</p>"),
                               HTML("<p>main motor:</p>"),
                               br(),
                               sliderInput("max_power", NULL, 5, 9, 7, post = " MW"),
                               HTML("<p>Choose Top Speed attained:</p>"),
                               sliderInput("max_speed", NULL, 16, 24, 20, post = " kts")))),
                column(4,
                       # Other known reference ====
                       div(class = "ui card",
                           div(class = "content",
                               uiicon("settings"),
                               "Other Reference"),
                           div(class = "content",
                               actionButton("OKR_jet_but", "Jet"),
                               actionButton("OKR_prop_but", "Prop"),
                               br(),br(),
                               HTML("<p>Choose a known power</p>"),
                               HTML("<p>(propulsion) and speed</p>"),
                               HTML("<p>match:</p>"),
                               br(),
                               sliderInput("OKR_power", NULL, 50, 5000, 500, post = " kW"),
                               sliderInput("OKR_speed", NULL, 0.5, 18, 10, post = " kts")))),
                column(3, br()))
              )))),

    br(),
    br(),
    fluidPage(
      semanticPage(
        fluidRow(
          column(4,
                   div(class = "ui horizontal divider", uiicon("settings"), "Range"),
                 plotlyOutput("range_plot")),
          column(4,
                   div(class = "ui horizontal divider", uiicon("settings"), "Endurance"),
                 plotlyOutput("endurance_plot")),
          column(4,
                   div(class = "ui horizontal divider", uiicon("settings"), "Power"),
                 plotlyOutput("power_plot"))
        )

      ))
    ))
  )
