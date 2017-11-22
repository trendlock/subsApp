# Libraries ----
library(shiny)
library(tidyverse)
library(lubridate)


library(shinyBS)

library(shinyWidgets)

library(plotly)

library(shinythemes)

library(shinycssloaders)

#devtools::install_github("Appsilon/shiny.semantic")
library(shiny.semantic)

#devtools::install_github("nstrayer/shinysense")
library(shinysense)



# loading css

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"



# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    theme = shinytheme("united"),
    title = "App Title",
    

    # Tab 1 ====
    tabPanel(
      title = "First Tab Title",
      
     
      
      semanticPage(
        
          
        div(class = "ui grid",
            
            # blank col?
            #div(class = "one wide column"),
            
            # eff plot ====
            div(class = "seven wide column",
                div(class = "ui horizontal divider", uiicon("tag"), "Efficiency Assumption",  bsButton("zoom_eff", "Zoom", size = "extra-small")),
                uisegment(plotlyOutput("eff_plot", height = "200px") %>% withSpinner()),
                
                div(class = "ui horizontal divider", uiicon("tag"), "Toggle Input"),
                
                uisegment(align = "center",
                          bsButton("tog_input", label = "Toggle Jet or Prop input",
                                   block = F, type = "toggle", value = F)),
                
                
                div(class = "ui horizontal divider", textOutput("message_drawr")),
                uisegment(shinydrawrUI("drawr_plot") %>% withSpinner()),
                
                # pop up window
                bsModal("modalExample", "Efficiency Assumption", "zoom_eff", size = "large",
                        plotlyOutput("eff_plot_modal") %>% withSpinner())
                
            ),
            
            # second col
            div(class = "nine wide column",
                div(class = "ui horizontal divider", uiicon("tag"), "Assumptions"),
                
                fluidRow(
                  # Hotel Load slider ====
                  column(4, 
                         div(class = "ui card",
                             div(class = "content",
                                 div(class = "right floated meta", "(kW)"),
                                 uiicon("settings"),
                                 "Hotel Load"),
                             div(class = "content",
                                 sliderInput("hotel_load", NULL, 75, 300, 200, post = " kW")),
                             div(class = "extra content", "other info"))),
                  # Onborded Batt  ====
                  column(4, 
                         div(class = "ui card",
                             div(class = "content",
                                 div(class = "right floated meta", "(Tonnes)"),
                                 uiicon("settings"),
                                 "Batteries"),
                             div(class = "content",
                                 sliderInput("onboarded_batt", "(Onboarded)", 100, 700, 500, post = " Tonnes")))),
                  # Other ref slider ====
                  column(4, 
                         div(class = "ui card",
                             div(class = "content",
                                 div(class = "right floated meta", " "),
                                 uiicon("settings"),
                                 "Battery Energy Density"),
                             div(class = "content",
                                 sliderInput("batt_energy_MJ_kg", NULL, 0.08, 0.8, 0.46, step = 0.02, post = " MJ/kg"),
                                 sliderInput("batt_energy_Wh_kg", NULL, 22.2, 222, 124, post = " Wh/kg")))),
                  
                  # select one method
                  
                  br(), br(), br(), br(),br(), br(), br(), br(),br(), br(), br(), br(),
                  
                  column(12, 
                         div(class = "ui horizontal divider", uiicon("settings"), "Power Reference Point")
                  ),
                  column(4, align = "center",
                         uisegment(
                           div(class = "ui horizontal divider", uiicon("settings"), "Pick Method"),
                           radioGroupButtons(inputId = "pick_method", 
                                             choices = c("Hotel Load Match",  
                                                         "Top Speed + Power",  
                                                         "Other Known Reference"), 
                                             selected = "Hotel Load Match",
                                             direction = "vertical", justified = T),
                           div(class = "ui horizontal divider", uiicon("settings"), "Pick System"),
                           radioGroupButtons(inputId = "pick_system", 
                                             choices = c("Jet",  
                                                         "Propeller"), 
                                             selected = "Jet",
                                             direction = "vertical", justified = T)
                         )),
                  column(8,
                         div(class = "ui raised segment",
                             bsCollapse(id = "pwr_ref_point_collapse", 
                                        open = "Hotel Load Match",
                                        bsCollapsePanel("Hotel Load Match", 
                                                        "Choose speed at which Hotel Load = Power Drawn by propulsion systems:",
                                                        sliderInput("HLM_patrol_speed", NULL, 0.5, 7, 5, step = 0.5, post = " kts")),
                                        bsCollapsePanel("Top Speed + Power",
                                                        "Choose max power of main motor:",
                                                        sliderInput("max_power", NULL, 5, 9, 7, post = " MW"),
                                                        "Choose Top Speed attained:",
                                                        sliderInput("max_speed", NULL, 16, 24, 20, post = " kts")),
                                        
                                        bsCollapsePanel("Other Known Reference", 
                                                        "Choose a known power (propulsion) and speedmatch:",
                                                        sliderInput("OKR_power", NULL, 50, 5000, 500, post = " kW"),
                                                        sliderInput("OKR_speed", NULL, 0.5, 18, 10, post = " kts"))))
                  ))),
            
            fluidPage(
              fluidRow(
                column(4, align="center",
                       div(class = "ui horizontal divider", uiicon("settings"), "Range", bsButton("zoom_range", "Zoom", size = "extra-small")),
                       plotlyOutput("range_plot") %>% withSpinner()),
                
                # pop up window
                bsModal("modalExample1", "Range", "zoom_range", size = "large",
                        plotlyOutput("range_plot_modal") %>% withSpinner()),
                
                
                column(4, align="center",
                       div(class = "ui horizontal divider", uiicon("settings"), "Endurance", bsButton("zoom_endurance", "Zoom", size = "extra-small")),
                       plotlyOutput("endurance_plot") %>% withSpinner()),
                
                # pop up window
                bsModal("modalExample2", "endurance", "zoom_endurance", size = "large",
                        plotlyOutput("endurance_plot_modal") %>% withSpinner()),
                
                
                column(4, align="center",
                       #uiheader("h", , icon = uiicon("settings")),
                       
                       div(class = "ui horizontal divider", uiicon("settings"), "Power", bsButton("zoom_power", "Zoom", size = "extra-small")),
                       plotlyOutput("power_plot") %>% withSpinner()
                ),
                
                # pop up window
                bsModal("modalExample3", "Power", "zoom_power", size = "large",
                        plotlyOutput("power_plot_modal") %>% withSpinner())
                
              )
              
            )
        ),
      
        br(), br(),br(),
        div(class = "ui horizontal divider", uiicon("tag"), "References"),
        "tidyverse, shinyBS, shiny.semantic, Plotly, shinyWidgets, shinysense"
        
      )
        )
      
    )
)


