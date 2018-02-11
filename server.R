
# Libraries ----
library(shiny)
library(tidyverse)
library(glue)
library(lubridate)
library(shinyBS)



library(plotly)

# devtools::install_github("rosseji/shiny.semantic@develop")
library(shiny.semantic)

# devtools::install_github("nstrayer/shinysense")
library(shinysense)

# devtools::install_github("trendlock/submarines", auth_token = read_rds("extdata/gh_token.rds"))
library(submarines)


# devtools::install_github("trendlock/fetch", auth_token = read_rds("extdata/gh_token.rds"))
library(fetch)




# Load Data ----

df <- read_rds("extdata/default_subs_df.rds")
app <- "subsApp"

# Server ====

shinyServer(function(input, output, session) {
  
  # pre user input ----
 
  
  # picks which  batt density to choose... ----
  
  
  batt_dens <- reactive({
    input$batt_energy_MJ_kg
  })


  # pick Power Reference Point system and method ====
  
  user_inputs <- reactiveValues(system = "jet", method = "hotel match")
  
  
  observeEvent(input$pwr_ref_point_collapse, {

    if (input$pwr_ref_point_collapse == "Hotel Load Match") {
      user_inputs$method <- "hotel match"
      
    } else if (input$pwr_ref_point_collapse == "Top Speed + Power") {
      user_inputs$method <- "max power"

    } else {
      user_inputs$method <- "other reference"
      
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
  

  
  # render draw message for toggle
  
  observeEvent(input$tog_input,{
    
    if(input$tog_input == "Input Jet Effieciency") {
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
  observeEvent(input$replot_all, {
  
      new_df <- df
    
        
      if(input$tog_input == "Input Jet Effieciency") {
       
         if(is.null(drawChart())) {
          drawn_vals <- df$eff.jet
        } else {
          drawn_vals <- drawChart()
          drawn_vals <- c(drawn_vals, tail(drawn_vals, 1))
        }
        
      } else {
        if(is.null(drawChart())) {
          ddrawn_vals <- df$eff.prop
        } else {
          drawn_vals <- drawChart()
          drawn_vals <- c(drawn_vals, tail(drawn_vals, 1))
        }
      }
  
      

      
      
      

  
      # a bodge to be fixed....?
     # drawn_vals <- c(drawn_vals, tail(drawn_vals, 1))
  

      
      # write drawns vals 1 ====
      sys_ <- if(input$tog_input == "Input Jet Effieciency") "jet" else "prop"
      drawn_vals_pth <- glue("saved_{sys_}_input_{now() %>% as.numeric}.rds")
      write_rds(drawn_vals, drawn_vals_pth)

      
      print(input$tog_input)
      #  drawing as jet input ======================
      if(input$tog_input == "Input Jet Effieciency") {
  
        status_jet <- try(read_rds("saved_jet_input.rds"))
        status_prop <- try(read_rds("saved_prop_input.rds"))
        
        
        # write fresh jet input
        write_rds(drawn_vals, "saved_jet_input.rds")
  
        # add drawn data
        new_df <- mutate(new_df, eff.jet = drawn_vals)
        
        #print(new_df)
        # look for saved prop data
        if (class(status_prop) == "numeric") {
          new_df <- mutate(new_df, eff.prop = status_prop)
        }  else {
          new_df <- mutate(new_df, eff.prop = df$eff.prop)
        }
        
      } else { # drawing as prop input ======================
        
        status_jet <- try(read_rds("saved_jet_input.rds"))
        status_prop <- try(read_rds("saved_prop_input.rds"))
        
        
        # if not found, write fresh
        write_rds(drawn_vals, "saved_prop_input.rds")
  
        # add drawn data
        new_df <- mutate(new_df, eff.prop = drawn_vals)
        
        print(class(status_jet))
        # look for saved jet data
        if (class(status_jet) == "numeric") {
          new_df <- mutate(new_df, eff.jet = status_jet)
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
        
        eff_plot_df <- eff_plot_df %>%
          mutate(cat = case_when(
            cat ==  "eff.prop" ~ "Propeller",
            cat ==  "eff.jet" ~ "Jet"
          ))
        
        
        ggplot(eff_plot_df, aes(x = kts, y = val, col = cat))+
          geom_line()+
          theme_minimal() +
          scale_y_continuous(breaks=seq(0, 1, 0.2),
                             name = "Efficiency") +
          scale_x_continuous(breaks=seq(0, 20, 2))
        
        ggplotly()
  
      })
      
      
      #  > Endurance  plot
      endurance_plot <- reactive({
        end_plot_df <- df_react() %>%
          filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour"))
        
        end_plot_df <- end_plot_df %>%
          mutate(cat = case_when(
            cat ==  "endurance.prop.hour" ~ "Propeller",
            cat ==  "endurance.jet.hour" ~ "Jet"
          ))
        
        y_max <- roundUpNice(max(pull(end_plot_df, val)))
        
        ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
          geom_line() +
          theme_minimal() +
          scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                             name = "hours") +
          scale_x_continuous(breaks=seq(0, 20, 2)) +
          if (!input$end_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
        
        
        ggplotly()%>%
          layout(legend = list(orientation = 'v',y =  0.99, x = 0.7))
        
      })
      
      # >  Range   plot
      range_plot <- reactive({
        range_plot_df <- df_react() %>%
          filter(cat %in% c("range.prop", "range.jet"))
        
        range_plot_df <- range_plot_df %>%
          mutate(cat = case_when(
            cat ==  "range.prop" ~ "Propeller",
            cat ==  "range.jet" ~ "Jet"
          ))
        
        y_max <- roundUpNice(max(pull(range_plot_df, val)))
        
        ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
          geom_line()+
          theme_minimal()+
          scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                             name = "nm") +
          scale_x_continuous(breaks=seq(0, 20, 2)) +
          if (!input$range_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
        
        ggplotly()%>%
          layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))
        
      })
      
      
      
      #  > Pwer  plot
      power_plot <- reactive({
        power_plot_df <- df_react() %>%
          filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop"))
        
        power_plot_df <- power_plot_df %>%
          mutate(cat = case_when(
            cat == "hotel" ~ "Hotel Load",
            cat ==  "power.mob.drawn.prop" ~ "Propulsion Power Drawn Propeller",
            cat ==  "power.mob.drawn.jet" ~ "Propulsion Power Drawn Jet",
            cat ==  "power.mob.req" ~ "Thrust Power Required"
          ))
        
        y_max <- roundUpNice(max(pull(power_plot_df, val)))
        
        ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
          geom_line() +
          theme_minimal() +
          scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                             name = "hours") +
          scale_x_continuous(breaks=seq(0, 20, 2)) +
          if (!input$power_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
        
        
        
        ggplotly()%>%
          layout(legend = list(orientation = 'v', y = 0.8, x = 0.01))
        
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
    
      
      # AWS push 1 =====
      
      fetch::push_to_cloud(drawn_vals_pth, drawn_vals_pth, "subs-drawn-eff", app)
      
      
  })
  
  
  # for other sliders.... re plot all
  
  
  
  #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  observeEvent(drawChart(), {
    
    new_df <- df
    drawn_vals <- drawChart()
    
    # a bodge to be fixed....?
    drawn_vals <- c(drawn_vals, tail(drawn_vals, 1))
    
    
    
    # write drawns vals 2 ====
    sys_ <- if(input$tog_input == "Input Jet Effieciency") "jet" else "prop"
    drawn_vals_pth <- glue("saved_{sys_}_input_{now() %>% as.numeric}.rds")
    write_rds(drawn_vals, drawn_vals_pth)

    
    print(input$tog_input)
    #  drawing as jet input ======================
    if(input$tog_input == "Input Jet Effieciency") {
      
      status_jet <- try(read_rds("saved_jet_input.rds"))
      status_prop <- try(read_rds("saved_prop_input.rds"))
      
      
      # write fresh jet input
      write_rds(drawn_vals, "saved_jet_input.rds")
      
      # add drawn data
      new_df <- mutate(new_df, eff.jet = drawn_vals)
      
      #print(new_df)
      # look for saved prop data
      if (class(status_prop) == "numeric") {
        new_df <- mutate(new_df, eff.prop = status_prop)
      }  else {
        new_df <- mutate(new_df, eff.prop = 1 - log(kts + 3)/6 + 0.10 - (kts/500))
      }
      
    } else { # drawing as prop input ======================
      
      status_jet <- try(read_rds("saved_jet_input.rds"))
      status_prop <- try(read_rds("saved_prop_input.rds"))
      
      
      # if not found, write fresh
      write_rds(drawn_vals, "saved_prop_input.rds")
      
      # add drawn data
      new_df <- mutate(new_df, eff.prop = drawn_vals)
      
      print(class(status_jet))
      # look for saved jet data
      if (class(status_jet) == "numeric") {
        new_df <- mutate(new_df, eff.jet = status_jet)
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
      
      eff_plot_df <- eff_plot_df %>%
        mutate(cat = case_when(
          cat ==  "eff.prop" ~ "Propeller",
          cat ==  "eff.jet" ~ "Jet"
        ))
      
      
      ggplot(eff_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line()+
        theme_minimal() +
        scale_y_continuous(breaks=seq(0, 1, 0.2),
                           name = "Efficiency") +
        scale_x_continuous(breaks=seq(0, 20, 2))
      
      ggplotly()
      
    })
    
    
    #  > Endurance  plot
    endurance_plot <- reactive({
      end_plot_df <- df_react() %>%
        filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour"))
      
      end_plot_df <- end_plot_df %>%
        mutate(cat = case_when(
          cat ==  "endurance.prop.hour" ~ "Propeller",
          cat ==  "endurance.jet.hour" ~ "Jet"
        ))
      
      y_max <- roundUpNice(max(pull(end_plot_df, val)))
      
      ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line() +
        theme_minimal() +
        scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                           name = "hours") +
        scale_x_continuous(breaks=seq(0, 20, 2)) +
        if (!input$end_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
      
      
      ggplotly()%>%
        layout(legend = list(orientation = 'v',y =  0.99, x = 0.7))
      
    })
    
    # >  Range   plot
    range_plot <- reactive({
      range_plot_df <- df_react() %>%
        filter(cat %in% c("range.prop", "range.jet"))
      
      range_plot_df <- range_plot_df %>%
        mutate(cat = case_when(
          cat ==  "range.prop" ~ "Propeller",
          cat ==  "range.jet" ~ "Jet"
        ))
      
      y_max <- roundUpNice(max(pull(range_plot_df, val)))
      
      ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line()+
        theme_minimal()+
        scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                           name = "nm") +
        scale_x_continuous(breaks=seq(0, 20, 2)) +
        if (!input$range_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
      
      ggplotly()%>%
        layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))
      
    })
    
    
    
    #  > Pwer  plot
    power_plot <- reactive({
      power_plot_df <- df_react() %>%
        filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop"))
      
      power_plot_df <- power_plot_df %>%
        mutate(cat = case_when(
          cat == "hotel" ~ "Hotel Load",
          cat ==  "power.mob.drawn.prop" ~ "Propulsion Power Drawn Propeller",
          cat ==  "power.mob.drawn.jet" ~ "Propulsion Power Drawn Jet",
          cat ==  "power.mob.req" ~ "Thrust Power Required"
        ))
      
      y_max <- roundUpNice(max(pull(power_plot_df, val)))
      
      ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
        geom_line() +
        theme_minimal() +
        scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                           name = "hours") +
        scale_x_continuous(breaks=seq(0, 20, 2)) +
        if (!input$power_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
      
      
      
      ggplotly()%>%
        layout(legend = list(orientation = 'v', y = 0.8, x = 0.01))
      
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
    
    
    # AWS push 2 =====
    
    fetch::push_to_cloud(drawn_vals_pth, drawn_vals_pth, "subs-drawn-eff", app)
    
    
  })
  
  
  # >  Eff  plot
  eff_plot <- reactive({
    eff_plot_df <- df_react() %>%
      filter(cat %in% c("eff.jet", "eff.prop"))
    
    
    eff_plot_df <- eff_plot_df %>%
      mutate(cat = case_when(
        cat ==  "eff.prop" ~ "Propeller",
        cat ==  "eff.jet" ~ "Jet"
      ))
    
    
    ggplot(eff_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line() +
      theme_minimal() +
      scale_y_continuous(breaks = seq(-.2, 1.2, 0.2),
                         name = "Efficiency") +
      scale_x_continuous(breaks=seq(0, 20, 2))
    
    
    ggplotly() %>%
      layout(legend = list(orientation = 'v', label = NULL))
  })
  
  
  #  > Endurance  plot
  endurance_plot <- reactive({
    end_plot_df <- df_react() %>%
      filter(cat %in% c("endurance.prop.hour", "endurance.jet.hour"))
    
    end_plot_df <- end_plot_df %>%
      mutate(cat = case_when(
        cat ==  "endurance.prop.hour" ~ "Propeller",
        cat ==  "endurance.jet.hour" ~ "Jet"
      ))
    
    
    y_max <- roundUpNice(max(pull(end_plot_df, val)))
    
    ggplot(end_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line() +
      theme_minimal() +
      scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                         name = "hours") +
      scale_x_continuous(breaks=seq(0, 20, 2)) +
      if (!input$end_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
    
    
    ggplotly()%>%
      layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))
    
  })
  
  # >  Range   plot
  range_plot <- reactive({
    range_plot_df <- df_react() %>%
      filter(cat %in% c("range.prop", "range.jet"))
    
    
    
    range_plot_df <- range_plot_df %>%
      mutate(cat = case_when(
        cat ==  "range.prop" ~ "Propeller",
        cat ==  "range.jet" ~ "Jet"
      ))
    
    y_max <- roundUpNice(max(pull(range_plot_df, val)))
    
    ggplot(range_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line()+
      theme_minimal()+
      scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                         name = "nm") +
      scale_x_continuous(breaks=seq(0, 20, 2)) +
      if (!input$range_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
    
    
    ggplotly()%>%
      layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))
    
  })
  
  
  
  #  > Pwer  plot
  power_plot <- reactive({
    power_plot_df <- df_react() %>%
      filter(cat %in% c("hotel","power.mob.drawn.jet", "power.mob.req", "power.mob.drawn.prop"))
    
    
    power_plot_df <- power_plot_df %>%
      mutate(cat = case_when(
        cat == "hotel" ~ "Hotel Load",
        cat ==  "power.mob.drawn.prop" ~ "Propulsion Power Drawn Propeller",
        cat ==  "power.mob.drawn.jet" ~ "Propulsion Power Drawn Jet",
        cat ==  "power.mob.req" ~ " Thrust Power Required"
      ))
    y_max <- roundUpNice(max(pull(power_plot_df, val)))
    
    ggplot(power_plot_df, aes(x = kts, y = val, col = cat))+
      geom_line() +
      theme_minimal() +
      scale_y_continuous(breaks=seq(0, y_max, (y_max/10)),
                         name = "kW") +
      scale_x_continuous(breaks=seq(0, 20, 2)) +
      if (!input$power_plot_label) geom_blank() else geom_text(aes(label = round(val, digits = 1)), nudge_y = 2, angle = 45)
    
    
    
    ggplotly()%>%
      layout(legend = list(orientation = 'v', y = 0.99, x = 0.01))
    
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
  
  
  
  # restore default values 
  
  
  observeEvent(input$restore_default_assum,  {

    if (input$restore_default_assum) {
      updateSliderInput(session,
                        "hotel_load", NULL, 150, 75, 300)

      updateSliderInput(session,
                        "onboarded_batt", "(Onboarded)", 500, 100, 700)

      updateSliderInput(session,
                        "batt_energy_MJ_kg", NULL, 0.14, 0.08, 0.8, step = 0.02)

    }


  })

  observeEvent(input$restore_defaults_power_ref,  {

    if(input$restore_defaults_power_ref) {
      updateSliderInput(session,
                        "HLM_patrol_speed", NULL, 2.5, 0.5, 7, step = 0.5)

      updateSliderInput(session,
                        "max_power", NULL, 7, 5, 9)

      updateSliderInput(session,
                        "max_speed", NULL, 18, 16, 20,  step = 0.25)
      updateSliderInput(session,
                        "OKR_power", NULL, 500, 50, 5000)

      updateSliderInput(session,
                        "OKR_speed", NULL, 10, 0.5, 18)

    }

    })
  
})
