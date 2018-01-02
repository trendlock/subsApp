
# Libraries ----

# on CRAN
library(shiny)
library(tidyverse)
library(glue)
library(lubridate)

library(plotly)

library(submarines)



# sends email on startup



# Load Data ----

df <- read_rds("extdata/default_subs_df.rds")
app <- "subsApp"

# Server ====

shinyServer(function(input, output, session) {
  
  

  
  df_draw <- df %>%
    mutate(eff.prop = NA)

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
  
  
  observeEvent(drawChart(), {
    
    
    if (input$tog_input == "Input Jet 1") drawChart() %>% write_rds("input_jet_1.rds")
    if (input$tog_input == "Input Jet 2") drawChart() %>% write_rds("input_jet_2.rds")
    if (input$tog_input == "Input Jet 3") drawChart() %>% write_rds("input_jet_3.rds")
    if (input$tog_input == "Input Propeller 1") drawChart() %>% write_rds("input_prop_1.rds")
    if (input$tog_input == "Input Propeller 2") drawChart() %>% write_rds("input_prop_2.rds")
    if (input$tog_input == "Input Propeller 3") drawChart() %>% write_rds("input_prop_3.rds")
    
    
    
    
    input_jet_1 <- tryCatch(read_rds("input_jet_1.rds"))
    if (class(input_jet_1) != "numeric") {
      input_jet_1 <- rep(NA, 40)
    } else {
      input_jet_1 <- lm(c(input_jet_1, tail(input_jet_1, 1)) ~ poly(pull(df, kts), input$num1))$fitted.values
    }
    
    input_jet_2 <- try(read_rds("input_jet_2.rds"))
    if (class(input_jet_2) != "numeric") {
      input_jet_2 <- rep(NA, 40)
    } else {
      input_jet_2 <- lm(c(input_jet_2, tail(input_jet_2, 1)) ~ poly(pull(df, kts), input$num2))$fitted.values
    }
    
    input_jet_3 <- try(read_rds("input_jet_3.rds"))
    if (class(input_jet_3) != "numeric") {
      input_jet_3 <- rep(NA, 40)
    } else {
      input_jet_3 <- lm(c(input_jet_3, tail(input_jet_3, 1)) ~ poly(pull(df, kts), input$num3))$fitted.values
    }
    
    input_prop_1 <- try(read_rds("input_prop_1.rds"))
    if (class(input_prop_1) != "numeric") {
      input_prop_1 <- rep(NA, 40)
    } else {
      input_prop_1 <- lm(c(input_prop_1, tail(input_prop_1, 1)) ~ poly(pull(df, kts), input$num4))$fitted.values
    }
    
    input_prop_2 <- try(read_rds("input_prop_2.rds"))
    if (class(input_prop_2) != "numeric") {
      input_prop_2 <- rep(NA, 40)
    } else {
      input_prop_2 <- lm(c(input_prop_2, tail(input_prop_2, 1)) ~ poly(pull(df, kts), input$num5))$fitted.values
    }
    
    input_prop_3 <- try(read_rds("input_prop_3.rds"))
    if (class(input_prop_3) != "numeric") {
      input_prop_3 <- rep(NA, 40)
    } else {
      input_prop_3 <- lm(c(input_prop_3, tail(input_prop_3, 1)) ~ poly(pull(df, kts),input$num6))$fitted.values
    }
    

    df <- tibble(
      kts = df$kts,
      input_jet_1,
      input_jet_2,
      input_jet_3,
      input_prop_1,
      input_prop_2,
      input_prop_3
    )  %>% 
      gather(cat, val, -kts)
    
    
    output$eff_plot <- renderPlotly({
      
      df <<- df
      ggplot(df, aes(x = kts, y = val, col = cat))+
        geom_line()+
        theme_minimal()
      
    })
    
    output$csv_curves <- downloadHandler(
      filename <- function() {
        paste('eff-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write_excel_csv(df, con)
      })
    
  })
  

  
})
