
# Libraries ----
library(shiny)
library(tidyverse)
library(lubridate)
library(shinyBS)



#devtools::install_github("ropensci/plotly")
library(plotly)

#devtools::install_github("Appsilon/shiny.semantic")
library(shiny.semantic)

#devtools::install_github("nstrayer/shinysense")
library(shinysense)

#devtools::install_github("trendlock/submarines", auth_token = read_rds("extdata/gh_token.rds"))
library(submarines)



# Load Data ----

df <- read_rds("extdata/default_subs_df.rds")


# Server ====

shinyServer(function(input, output, session) {
  
  # pre user input ----
  
  
  # picks which  batt density to choose... ----
  
  # defaults
  batt_dens <- reactive({
    input$batt_energy_MJ_kg
  })
  
  
  observeEvent(input$batt_energy_MJ_kg,{
    batt_dens <- input$batt_energy_MJ_kg
  })
  observeEvent(input$batt_energy_Wh_kg,{
    batt_dens <- input$batt_energy_Wh_kg
  })
  
  # pick Power Reference Point system and method ====
  
  user_inputs <- reactiveValues(system = "jet", method = "hotel match")
  
  
  
  # > Pick Method
  observeEvent(input$pick_method,{
    if (input$pick_method == "Hotel Load Match") {
      user_inputs$method <- "hotel match"
      updateCollapse(session, "pwr_ref_point_collapse", open = "Hotel Load Match")
    } else if (input$pick_method == "Top Speed + Power") {
      user_inputs$method <- "max power"
      updateCollapse(session, "pwr_ref_point_collapse", open = "Top Speed + Power")
    } else {
      user_inputs$method <- "other reference"
      updateCollapse(session, "pwr_ref_point_collapse", open = "Other Known Reference")
    }
  })
  
  # > Pick System
  observeEvent(input$pick_system,{
    if (input$pick_system == "Jet") {
      user_inputs$system <- "jet"
    } else {
      user_inputs$system <- "prop"
    }
  })
  
  
  
  # run computation ====
  df_react <- reactive({
    run_subs(df,
             input$hotel_load,
             input$onboarded_batt,
             batt_dens(),
             input$HLM_patrol_speed,
             input$max_speed,
             input$max_power,
             input$OKR_speed,
             input$OKR_power,
             user_inputs$system,
             user_inputs$method)
  })
  
  #  *** build plots ====
  
  # >  Eff  plot
  eff_plot <- reactive({
    eff_plot_df <- df_react() %>%
      filter(cat %in% c("eff.jet", "eff.prop"))
    
    ggplot(eff_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line()+
      theme_classic()
    
    ggplotly()#%>%
    #layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))
    
  })
  
  
  #  > Endurance  plot
  endurance_plot <- reactive({
    end_plot_df <- df_react() %>%
      filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour"))
    
    ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line()+
      #geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)+
      theme_classic()
    
    ggplotly()%>%
      layout(legend = list(orientation = 'v', y = 0.8, x = 0.5))
    
  })
  
  # >  Range   plot
  range_plot <- reactive({
    range_plot_df <- df_react() %>%
      filter(cat %in% c("range.prop", "range.jet"))
    
    ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line()+
      #geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)+
      theme_classic()
    
    ggplotly()%>%
      layout(legend = list(orientation = 'v', y = 0.9, x = 0.5))
    
  })
  
  
  
  #  > Pwer  plot
  power_plot <- reactive({
    power_plot_df <- df_react() %>%
      filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop"))
    
    ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line()+
      theme_classic()
    
    
    ggplotly()%>%
      layout(legend = list(orientation = 'v', y = 0.8, x = 0.1))
    
  })
  
  # *** end build plots =====
  
  
  # *** render plots  ====
  
  #  > Eff  plot
  output$eff_plot <- renderPlotly({
    eff_plot()
  })
  output$eff_plot_modal <- renderPlotly({
    eff_plot()
  })
  
  
  
  #  > Endurance  plot
  output$endurance_plot <- renderPlotly({
    endurance_plot()
  })
  output$endurance_plot_modal <- renderPlotly({
    endurance_plot()
  })
  
  
  #  > Range   plot
  output$range_plot <- renderPlotly({
    range_plot()
  })
  
  output$range_plot_modal <- renderPlotly({
    range_plot()
  })
  
  
  
  #  > Pwer  plot
  output$power_plot <- renderPlotly({
    power_plot()
  })
  output$power_plot_modal <- renderPlotly({
    power_plot()
  })
  
  
  # *** end plots ====
  
  
  # post User input -----
  
  
  # render draw message for toggle
  
  observeEvent(input$tog_input,{
    
    if(!input$tog_input) {
      mess_draw <- "Propose jet efficiency"
    } else {
      mess_draw <- "Propose propeller efficiency"
    }
    
    output$message_drawr <- renderText(
      mess_draw
    )
    
  })
  
  
  
  
  df_draw <- df %>%
    mutate(eff.prop = NA)
  
  
  
  #server side call of the drawr module
  
  
  
  drawChart <- callModule(
    shinydrawr,
    "drawr_plot",
    data = df_draw,
    draw_start = 0.5,
    x_key = "kts",
    y_key = "eff.prop",
    y_max = 1,
    y_min = 0
  )
  
  
  
  
  #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  observeEvent(drawChart(), {
    
    drawn_vals <- drawChart()
    
    
    message("drawn_vals")
    print(drawn_vals)
    print(length(drawn_vals))
    # a bodge to be fixed....
    drawn_vals <- c(drawn_vals, tail(drawn_vals, 1))
    
    
    
    
    
    # check status on saved vals
    status_jet <- safe_read_rds("saved_jet_input.rds")
    status_prop <- safe_read_rds("saved_prop_input.rds")
    
    
    
    #  drawing as jet input ======================
    if(!input$tog_input) {
      print("running drawing input for jet")
      
      
      # if not found, write fresh jet input
      if (is.null(status_jet["result"])) {
        write_rds(drawn_vals, "saved_jet_input.rds")
      } 
      
      
      # add drawn data
      new_df <- mutate(df, eff.jet = drawn_vals)
      
      print(new_df)
      # look for saved prop data
      if (is.null(status_prop["error"])) {
        new_df <- mutate(new_df, eff.prop = read_rds("saved_prop_input.rds"))
      }  else {
        new_df <- mutate(new_df, eff.prop = 1 - log(kts + 3)/6 + 0.10 - (kts/500))
      }
      
    } else { # drawing as prop input ======================
      
      print("running drawing input for prop")
      
      
      # if not found, write fresh
      if (is.null(status_prop["result"])) {
        write_rds(drawn_vals, "saved_prop_input.rds")
      } 
      
      # add drawn data
      new_df <- mutate(df, eff.prop = drawn_vals)
      
      # look for saved jet data
      if (is.null(status_jet["error"])) {
        new_df <- mutate(new_df, eff.jet = read_rds("saved_jet_input.rds"))
      }  else {
        new_df <- mutate(new_df, eff.jet = pull(df, eff.jet))
      }
    }
    
    # render draw mmessage
    
    
    
    
    # Add hotel load val
    new_df <- new_df %>% 
      mutate(hotel = input$hotel_load)
    
    
    # smooth as
    fit_poly_mod_jet <- lm(pull(new_df, eff.jet) ~ poly(pull(new_df, kts), 3))
    fit_poly_mod_prop <- lm(pull(new_df, eff.prop) ~ poly(pull(new_df, kts), 3))
    new_df <- new_df %>%
      mutate(eff.jet = fit_poly_mod_jet$fitted.values,
             eff.prop = fit_poly_mod_prop$fitted.values)
    
    
    
    
    # run computation ====
    df_react <- reactive({
      run_subs(new_df,
               input$hotel_load,
               input$onboarded_batt,
               batt_dens(),
               input$HLM_patrol_speed,
               input$max_speed,
               input$max_power,
               input$OKR_speed,
               input$OKR_power,
               user_inputs$system,
               user_inputs$method)
    })
    
    
    # >> build plots ====
    # >  Eff  plot
    eff_plot <- reactive({
      eff_plot_df <- df_react() %>%
        filter(cat %in% c("eff.jet", "eff.prop"))
      
      ggplot(eff_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line()+
        theme_classic()
      
      ggplotly()#%>%
      #layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))
      
    })
    
    
    #  > Endurance  plot
    endurance_plot <- reactive({
      end_plot_df <- df_react() %>%
        filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour"))
      
      ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line()+
        #geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)+
        theme_classic()
      
      ggplotly()%>%
        layout(legend = list(orientation = 'v', y = 0.8, x = 0.5))
      
    })
    
    # >  Range   plot
    range_plot <- reactive({
      range_plot_df <- df_react() %>%
        filter(cat %in% c("range.prop", "range.jet"))
      
      ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line()+
        #geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)+
        theme_classic()
      
      ggplotly()%>%
        layout(legend = list(orientation = 'v', y = 0.9, x = 0.5))
      
    })
    
    
    
    #  > Pwer  plot
    power_plot <- reactive({
      power_plot_df <- df_react() %>%
        filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop"))
      
      ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line()+
        theme_classic()
      
      
      ggplotly()%>%
        layout(legend = list(orientation = 'v', y = 0.8, x = 0.1))
      
    })
    
    # >> end build plots ====
    
    # >> render plots  Drawn line ====
    #  > Eff  plot
    output$eff_plot <- renderPlotly({
      eff_plot()
    })
    output$eff_plot_modal <- renderPlotly({
      eff_plot()
    })
    
    
    
    #  > Endurance  plot
    output$endurance_plot <- renderPlotly({
      endurance_plot()
    })
    output$endurance_plot_modal <- renderPlotly({
      endurance_plot()
    })
    
    
    #  > Range   plot
    output$range_plot <- renderPlotly({
      range_plot()
    })
    
    output$range_plot_modal <- renderPlotly({
      range_plot()
    })
    
    
    
    #  > Pwer  plot
    output$power_plot <- renderPlotly({
      power_plot()
    })
    output$power_plot_modal <- renderPlotly({
      power_plot()
    })
    
    # >> end render drawn ====
  })
  
})
