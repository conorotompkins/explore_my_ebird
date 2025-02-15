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
      distinct(submission_id, obs_date_y, obs_date_ym, obs_date_yw, obs_date_m, obs_date_w) |> 
      rename(`Year` = obs_date_y,
             `Year-month` = obs_date_ym,
             `Year-week` = obs_date_yw,
             Month = obs_date_m,
             Week = obs_date_w)
    
  })
  
  observeEvent(obs_data(), {
    
    var_cols <- obs_data() |> 
      select(starts_with("Year"))
    
    updateVarSelectizeInput(inputId = "checklist_date_selector",
                            data = var_cols)
    
  })
  
  #checklists
  
  output$obs_linechart <- renderPlot({
    
    req(input$checklist_date_selector)
    
    obs_data() |> 
      count(!!input$checklist_date_selector) |> 
      mutate(n_cumsum = cumsum(n)) |> 
      ggplot(aes(!!input$checklist_date_selector, n_cumsum)) +
      geom_line() +
      labs(title = "Checklist count over time")
    
  })
  
  heatmap_cols <- reactive({
    
    obs_data() |> 
      select(Year, Week, Month)
    
  })
  
  observeEvent(obs_data(), {
    
    var_cols <- heatmap_cols()
    
    updateVarSelectizeInput(inputId = "checklist_date_selector_x",
                            data = var_cols,
                            selected = "Month")
    
  })
  
  observeEvent(obs_data(), {
    
    var_cols <- heatmap_cols()
    
    updateVarSelectizeInput(inputId = "checklist_date_selector_y",
                            data = var_cols,
                            selected = "Year")
    
  })
  
  output$checklist_heatmap <- renderPlot({
    
    req(input$checklist_date_selector_x != input$checklist_date_selector_y)
    
    obs_data() |> 
      count(!!input$checklist_date_selector_x, !!input$checklist_date_selector_y) |> 
      complete(!!input$checklist_date_selector_x, !!input$checklist_date_selector_y) |> 
      replace_na(list(n = 0)) |> 
      ggplot(aes(!!input$checklist_date_selector_x, !!input$checklist_date_selector_y, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Checklist heatmap",
           fill = "Checklists")
    
  })
  
}