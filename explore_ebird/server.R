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
  rename(obs_date = date,
         species_count = count) |> 
  mutate(obs_date_ym = yearmonth(obs_date),
         obs_date_yw = yearweek(obs_date),
         obs_date_y = year(obs_date),
         obs_date_m = month(obs_date, label = TRUE, abbr = TRUE),
         obs_date_w = isoweek(obs_date),
         obs_date_wday = wday(obs_date, label = TRUE, abbr = TRUE),
         obs_date_hour = hour(time)) |> 
  arrange(submission_id)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  checklist_data <- reactive({
    
    my_data |> 
      select(submission_id, common_name, duration_min, common_name, obs_date_y, obs_date_ym, obs_date_yw, obs_date_m, obs_date_w, obs_date_wday, obs_date_hour) |> 
      rename(Year = obs_date_y,
             `Year-month` = obs_date_ym,
             `Year-week` = obs_date_yw,
             Month = obs_date_m,
             Week = obs_date_w,
             Weekday = obs_date_wday,
             Hour = obs_date_hour) |> 
      arrange(submission_id) |> 
      group_by(submission_id, common_name, Year, `Year-month`, `Year-week`, Month, Week, Weekday, Hour) |> 
      summarize(species_count = n(),
                duration = sum(duration_min)) |> 
      ungroup()
    
  })
  
  observeEvent(checklist_data(), {
    
    var_cols <- checklist_data() |> 
      select(Year, `Year-month`, `Year-week`)

    updateVarSelectizeInput(inputId = "checklist_date_selector",
                            data = var_cols)
    
  })
  
  #checklists
  
  output$obs_linechart <- renderPlot({
    
    req(input$checklist_date_selector)
    
    checklist_data() |> 
      distinct(submission_id, !!input$checklist_date_selector) |> 
      count(!!input$checklist_date_selector) |> 
      mutate(n_cumsum = cumsum(n)) |> 
      ggplot(aes(!!input$checklist_date_selector, n_cumsum)) +
      geom_line() +
      labs(title = "Checklist count over time")
    
  })
  
  heatmap_date_time_count <- reactive({
    
    checklist_data() |> 
      select(submission_id, common_name, !!input$checklist_date_selector_x, !!input$checklist_date_selector_y) |> 
      group_by(!!input$checklist_date_selector_x, !!input$checklist_date_selector_y) |> 
      summarize(checklist_count = n_distinct(submission_id),
                species_count = n_distinct(common_name)) |> 
      ungroup() |> 
      complete(!!input$checklist_date_selector_x, !!input$checklist_date_selector_y) |>
      replace_na(list(checklist_count = 0,
                      species_count = 0)) |> 
      rename(`Checklist count` = checklist_count,
             `Species count` = species_count)
    
  })
  
  heatmap_cols <- reactive({
    
    checklist_data() |> 
      select(Year, Month, Week, Weekday, Hour)
    
  })
  
  observeEvent(checklist_data(), {
    
    var_cols <- heatmap_cols()
    
    updateVarSelectizeInput(inputId = "checklist_date_selector_x",
                            data = var_cols,
                            selected = "Month")
    
  })
  
  observeEvent(checklist_data(), {
    
    var_cols <- heatmap_cols()
    
    updateVarSelectizeInput(inputId = "checklist_date_selector_y",
                            data = var_cols,
                            selected = "Year")
    
  })
  
  observeEvent(checklist_data(), {
    
    var_cols <- heatmap_date_time_count() |> 
      select(`Checklist count`, `Species count`)
    
    updateVarSelectizeInput(inputId = "checklist_metric_selector",
                            data = var_cols,
                            selected = "Checklist count")
    
  })
  
  #trigger modal dialog if axes are the same
  
  observeEvent(c(input$checklist_date_selector_x, input$checklist_date_selector_y), {

    if((input$checklist_date_selector_x == input$checklist_date_selector_y) && isTruthy(input$checklist_date_selector_x) && isTruthy(input$checklist_date_selector_y)) {
      showModal(modalDialog(
        title = "Oops!",
        "Choose different columns for each axis",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  output$checklist_heatmap <- renderPlot({
    
    req(input$checklist_date_selector_x != input$checklist_date_selector_y, input$checklist_metric_selector)

    heatmap_date_time_count() |> 
      ggplot(aes(!!input$checklist_date_selector_x, !!input$checklist_date_selector_y, fill = !!input$checklist_metric_selector)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Checklist heatmap",
           fill = input$checklist_metric_selector)
    
  })
  
}