# Libraries ----

# on CRAN
library(shiny)
library(tidyverse)
library(lubridate)
library(glue)
library(plotly)
library(shinyBS)
library(shinyWidgets)
library(rintrojs)
library(shinythemes)
library(shinycssloaders)



# remotes

devtools::install_github("nstrayer/shinysense")
devtools::install_github("rosseji/shiny.semantic@develop")


library(shiny.semantic)
library(shinysense)




# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    theme = shinytheme("united"),
    title = "SubsApp",
    
    
    
    # Tab 1 ====
    tabPanel(
      title = "Dashboard",
      
      # css ====
      #includeCSS("www/custom.css"),
      
      
      semanticPage(
        
        introjsUI(),
        
        
        div(class = "ui grid",
            div(class = "three wide column",
                
                actionButton("instructions", "Instructions", icon("question"), width = "100%"),
                
                
                div(class = "ui horizontal divider", uiicon("tag"), "Assumptions",
                    bsButton("restore_default_assum", "Restore Defaults", icon = uiicon("refresh"), size = "extra-small")),
                
                
                introBox(
                  div(class = "ui card",
                      div(class = "content",
                          uiicon("settings"),
                          "Hotel Load"),
                      div(class = "content",
                          sliderInput("hotel_load", NULL, 75, 300, 150, post = " kW"))),
                  div(class = "ui card",
                      div(class = "content",
                          uiicon("settings"),
                          "Batteries Onboarded"),
                      div(class = "content",
                          sliderInput("onboarded_batt", NULL, 100, 700, 500, post = " Tonnes"))),
                  
                  div(class = "ui card",
                      div(class = "content",
                          uiicon("settings"),
                          "Battery Energy Density"),
                      
                      div(class = "content",
                          sliderInput("batt_energy_MJ_kg", NULL, 0.08, 0.8, 0.14, step = 0.02, post = " MJ/kg"),
                          
                          uiOutput("batt_den"))),
                  
                         
                  # INSTRUCTION 1 ====
                  data.step = 1,
                  data.intro = "These three slide rules just need one instruction"),
                    
                uisegment(
                  bsButton("replot_all", "Replot All")),

                
                
                uisegment(align = "center",

                          tags$a(href='https://www.trendlock.com.au/',
                                 tags$img(src='tl_logo.png', width = "170")))

            ),
            
            # eff plot ====
            div(class = "seven wide column",
                div(class = "ui horizontal divider", uiicon("tag"), "Efficiency Assumption",  
                    bsButton("zoom_eff", "Zoom", size = "extra-small")),
                uisegment(plotlyOutput("eff_plot", height = "300px")),
                
                div(class = "ui horizontal divider", textOutput("message_drawr")),
                
               
                  uisegment(align = "center",
                            radioGroupButtons(inputId = "tog_input", 
                                              choices = c("Input Jet Effieciency",  
                                                          "Input Propeller Effieciency"), 
                                              selected = "Input Jet Effieciency", justified = T)),
                  
                 
                
            introBox(
                uisegment(height = "300px",
                          shinydrawrUI("drawr_plot") %>% withSpinner()),
                
                
                # INSTRUCTION 2 ====
                data.step = 2,
                data.intro = "Drag mouse to draw curve. It will only plot a curve that goes all teh way from the left to the right hand side."),
            
                # pop up window
                bsModal("modalExample", "Efficiency Assumption", "zoom_eff", size = "large",
                        plotlyOutput("eff_plot_modal") %>% withSpinner())
                
            ),
            
            # second col
            div(class = "six wide column",
                div(class = "ui horizontal divider", uiicon("settings"), "Power Reference Point",
                    bsButton("restore_defaults_power_ref", "Restore Defaults", icon = uiicon("refresh"), size = "extra-small")),
                
                column(12, align = "center",
                       introBox(
                         uisegment(
                           div(class = "ui horizontal divider", uiicon("settings"), "Pick System"),
                           radioGroupButtons(inputId = "pick_system", 
                                             choices = c("Jet",  
                                                         "Propeller"), 
                                             selected = "Jet",
                                             direction = "vertical", justified = T)),
                         
                         
                         # INSTRUCTION 3 ====
                         data.step = 3,
                         data.intro = "You can change which system you want to model..."),
                       
                       
                       
                       
                       div(class = "ui raised segment",
                           introBox(
                            div(class = "ui horizontal divider", uiicon("settings"), "Pick Method"),
                             bsCollapse(id = "pwr_ref_point_collapse", 
                                        open = "Hotel Load Match",
                                        bsCollapsePanel("Hotel Load Match", 
                                                        "Choose speed at which Hotel Load = Power Drawn by propulsion systems:",
                                                        sliderInput("HLM_patrol_speed", NULL, 0.5, 7, 2.5, step = 0.5, post = " kts")),
                                        bsCollapsePanel("Top Speed + Power",
                                                        "Choose max power of main motor:",
                                                        sliderInput("max_power", NULL, 5, 9, 7, step = 0.25, post = " MW"),
                                                        "Choose Top Speed attained:",
                                                        sliderInput("max_speed", NULL, 16, 20, 18, step = 0.25, post = " kts")),
                                        
                                        bsCollapsePanel("Other Known Reference", 
                                                        "Choose a known power (propulsion) and speedmatch:",
                                                        sliderInput("OKR_power", NULL, 50, 5000, 1500, post = " kW"),
                                                        sliderInput("OKR_speed", NULL, 0.5, 18, 8, post = " kts"))),
                            
                            # INSTRUCTION 4 ====
                            data.step = 4,
                            data.intro = "Choose one of the three methods.")
                       ),
                      
                       br(),
                       bsButton("info_source", "Reference"),
                       br(),
                       img(src = "eff.png", width = "400")
                       
                       

                       
                       
                )),
            
            
            fluidPage(
              introBox(
                fluidRow(
                
                  column(4, align="center",
                         div(class = "ui horizontal divider", uiicon("settings"), "Range", 
                             bsButton("zoom_range", "Zoom", size = "extra-small"),
                             bsButton("range_plot_label", "Add Label", size = "extra-small", type = "toggle", value = F)),
                         plotlyOutput("range_plot")),
                  
                  # pop up window
                  bsModal("modalExample1", "Range", "zoom_range", size = "large",
                          plotlyOutput("range_plot_modal") %>% withSpinner()),
                  
                  
                  column(4, align="center",
                         div(class = "ui horizontal divider", uiicon("settings"), "Endurance",
                             bsButton("zoom_endurance", "Zoom", size = "extra-small"),
                             bsButton("end_plot_label", "Add Label", size = "extra-small", type = "toggle", value = F)),
                         plotlyOutput("endurance_plot")),
                  
                  # pop up window
                  bsModal("modalExample2", "endurance", "zoom_endurance", size = "large",
                          plotlyOutput("endurance_plot_modal") %>% withSpinner()),
                  
                  
                  column(4, align="center",
                         #uiheader("h", , icon = uiicon("settings")),
                         
                         div(class = "ui horizontal divider", uiicon("settings"), "Power", 
                             bsButton("zoom_power", "Zoom", size = "extra-small"),
                             bsButton("power_plot_label", "Add Label", size = "extra-small", type = "toggle", value = F)),
                         plotlyOutput("power_plot")
                  ),
                  
                  # pop up window
                  bsModal("modalExample3", "Power", "zoom_power", size = "large",
                          plotlyOutput("power_plot_modal") %>% withSpinner())
                  

                ),
                
                # INSTRUCTION 5 ====
                data.step = 5,
                data.intro = "These plots show...")
              
              
                
            )
        ),
        
        br(), br(),br(),
        div(class = "ui horizontal divider", uiicon("tag"), "References"),
        "tidyverse, shinyBS, shiny.semantic, Plotly, shinyWidgets, shinysense"
        
      )
    )
    
  )
)


