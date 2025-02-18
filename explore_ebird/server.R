library(shiny)
library(tidyverse)
library(tsibble)
library(janitor)
library(here)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

# Define server logic required to draw a histogram
function(input, output, session) {
  
  user_data <- reactive({
    
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    
    read_csv(input$upload$datapath) |> 
      clean_names() |> 
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
    
  })
  
  checklist_data <- reactive({
    
    user_data() |> 
      select(submission_id, common_name, duration_min, common_name, obs_date, obs_date_y, obs_date_ym, obs_date_yw, obs_date_m, obs_date_w, obs_date_wday, obs_date_hour) |> 
      rename(Date = obs_date,
             Year = obs_date_y,
             `Year-month` = obs_date_ym,
             `Year-week` = obs_date_yw,
             Month = obs_date_m,
             Week = obs_date_w,
             Weekday = obs_date_wday,
             Hour = obs_date_hour) |> 
      arrange(submission_id) |> 
      group_by(submission_id, common_name, Date, Year, `Year-month`, `Year-week`, Month, Week, Weekday, Hour) |> 
      summarize(species_count = n(),
                duration = sum(duration_min)) |> 
      ungroup()
    
  })
  
  observeEvent(checklist_data(), {
    
    var_cols <- checklist_data() |> 
      select(Year, `Year-month`, `Year-week`, Date)
    
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
  
  #species detection
  
  species_detection_df <- reactive({
    
    user_data() |> 
      filter(all_obs_reported == 1) |> 
      select(submission_id,, protocol, state_province, location, starts_with("obs_date"),
             number_of_observers, duration_min, distance_traveled_km, common_name) |> 
      group_by(submission_id, protocol, state_province, location, obs_date_m, obs_date_wday, obs_date_hour,
               number_of_observers, duration_min, distance_traveled_km) |> 
      summarize(species_count = n_distinct(common_name)) |> 
      ungroup() |> 
      mutate(flag_travelling = !is.na(distance_traveled_km)) |> 
      replace_na(list(distance_traveled_km = 0)) |> 
      rename(`Distance traveled` = distance_traveled_km,
             Duration = duration_min,
             `Species detected` = species_count)
    
  })
  
  observeEvent(species_detection_df(), {
    
    vars <- species_detection_df() |>
      select(`Distance traveled`, Duration, `Species detected`)
    
    updateVarSelectizeInput(inputId = "effort_x_axis",
                            data = vars,
                            selected = "Distance traveled")
    
  })
  
  observeEvent(species_detection_df(), {
    
    vars <- species_detection_df() |>
      select(`Distance traveled`, Duration, `Species detected`)
    
    updateVarSelectizeInput(inputId = "effort_y_axis",
                            data = vars,
                            selected = "Duration")
    
  })
  
  #trigger modal dialog if axes are the same
  
  observeEvent(c(input$effort_x_axis, input$effort_y_axis), {
    
    if((input$effort_x_axis == input$effort_y_axis) && isTruthy(input$effort_x_axis) && isTruthy(input$effort_y_axis)) {
      showModal(modalDialog(
        title = "Oops!",
        "Choose different columns for each axis",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  output$effort_vs_species_count <- renderPlot({
    
    req(input$effort_x_axis != input$effort_y_axis)
    
    species_detection_df() |> 
      filter(flag_travelling == TRUE) |> 
      ggplot(aes(!!input$effort_x_axis, !!input$effort_y_axis, size = `Species detected`)) +
      geom_jitter(alpha = .3) +
      labs(title = "Checklist effort vs species detected",
           x = input$effort_x_axis,
           y = input$effort_y_axis,
           size = "Species detected")
    
  })
  
}