
# Libraries ----
library(shiny)
library(tidyverse)
library(lubridate)

#devtools::install_github("ropensci/plotly")
library(plotly)

#devtools::install_github("Appsilon/shiny.semantic")
library(shiny.semantic)

#devtools::install_github("nstrayer/shinysense")
library(shinysense)

#devtools::install_github("trendlock/submarines@subsApp2", auth_token = read_rds("extdata/gh_token.rds"))
library(submarines)



# Load Data ----

df <- read_rds("extdata/default_subs_df.rds")


# Server ====

shinyServer(function(input, output) {

  # pre user input ----

  # picks which  batt density to choose... ----

  # defaults
  batt_dens <- reactive({
    input$batt_energy_MJ_kg
  })


  observeEvent(input$batt_energy_MJ_kg,{
    batt_dens <- input$batt_energy_MJ_kg
  })
  observeEvent(input$batt_energy_MJ_kg,{
    batt_dens <- input$batt_energy_MJ_kg
  })

  # pick Power Reference Point system and method ====

  user_inputs <- reactiveValues(system = "jet", method = "hotel match")

  # > Hotel Match
  observeEvent(input$HL_jet_but,{
    user_inputs$system <- "jet"
    user_inputs$method <- "hotel match"
  })
  observeEvent(input$HL_prop_but,{
    user_inputs$system <- "prop"
    user_inputs$method <- "hotel match"
  })

  # > Top speed Pwer
  observeEvent(input$TSP_jet_but,{
    user_inputs$system <- "jet"
    user_inputs$method <- "max power"

  })
  observeEvent(input$TSP_prop_but,{
    user_inputs$system <- "prop"
    user_inputs$method <- "max power"
  })
  # > Other refernce
  observeEvent(input$OKR_jet_but,{
    user_inputs$system <- "jet"
    user_inputs$method <- "other reference"
  })
  observeEvent(input$OKR_prop_but,{
    user_inputs$system <- "prop"
    user_inputs$method <- "other reference"
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
      labs(title = "Efficiency Assumption") +
      theme_classic()

    ggplotly()%>%
      layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))

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


  #  > Endurance  plot
  output$endurance_plot <- renderPlotly({
    endurance_plot()
  })

  #  > Range   plot
  output$range_plot <- renderPlotly({
    range_plot()
  })



  #  > Pwer  plot
  output$power_plot <- renderPlotly({
    power_plot()
  })


  # *** end plots ====


  # post User input -----

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

    drawnValues <- drawChart()


    message("drawnValues")
    print(drawnValues)
    print(length(drawnValues))


    # a bodge to be fixed....
    drawnValues <- c(drawnValues, tail(drawnValues, 1))

    new_df <- df %>%
      mutate(eff.jet = drawnValues,
             eff.prop = 1 - log(kts + 3)/6 + 0.10 - (kts/500),
             input$hotel_load)



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
        labs(title = "Efficiency Assumption") +
        theme_classic()

      ggplotly()%>%
        layout(legend = list(orientation = 'v', y = 0.9, x = 0.7))

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


    #  > Endurance  plot
    output$endurance_plot <- renderPlotly({
      endurance_plot()
    })

    #  > Range   plot
    output$range_plot <- renderPlotly({
      range_plot()
    })

    #  > Pwer  plot
    output$power_plot <- renderPlotly({
      power_plot()
    })

    # >> end render drawn ====
  })

})
