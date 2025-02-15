library(shiny)
library(tidyverse)
library(tsibble)
library(janitor)
library(here)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

my_data_raw <- here("inputs/MyEBirdData.csv") |> 
  read_csv() |> 
  clean_names()

my_data <- my_data_raw |> 
  rename(obs_date = date) |> 
  mutate(obs_date_ym = yearmonth(obs_date),
         obs_date_yw = yearweek(obs_date),
         obs_date_y = year(obs_date),
         obs_date_m = month(obs_date, label = TRUE, abbr = TRUE),
         obs_date_w = isoweek(obs_date))

# Define server logic required to draw a histogram
function(input, output, session) {
  
  obs_data <- reactive({
    
    my_data |> 
      distinct(submission_id, obs_date_y, obs_date_ym, obs_date_yw) |> 
      rename(`Year` = obs_date_y,
             `Year-month` = obs_date_ym,
             `Year-week` = obs_date_yw)
    
  })
  
  observeEvent(obs_data(), {
    
    var_cols <- obs_data() |> 
      select(starts_with("Year"))
    
    updateVarSelectizeInput(inputId = "checklist_date_selector",
                            data = var_cols)
    
  })
  
  output$obs_linechart <- renderPlot({
    
    req(input$checklist_date_selector)
    
    obs_data() |> 
      count(!!input$checklist_date_selector) |> 
      mutate(n_cumsum = cumsum(n)) |> 
      ggplot(aes(!!input$checklist_date_selector, n_cumsum)) +
      geom_line() +
      labs(title = "Checklist count over time")
    
  })
  
}