library(shiny)
library(tidyverse)
library(tsibble)
library(janitor)
library(here)
library(reactable)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

species_exclude_df <- tibble(common_name = c("Muscovy Duck"))

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #trigger modal to upload user data
  observeEvent(input$trigger_upload_modal, {
    
    showModal(
      
      modalDialog(
        
        title = "Upload eBird data!",
        
        renderUI(
          
          fileInput(inputId = "upload",
                    label = "Upload eBird CSV",
                    accept = ".csv")
          
        ),
        
        easyClose = TRUE,
        footer = NULL
        
      )
      
    )
    
  })
  
  observeEvent(input$upload, {
    
    Sys.sleep(1)
    
    removeModal()
    
  })
  
  user_data_raw <- reactive({
    
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    
    #read in and clean up
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
  
  user_data <- reactive({
    
    #apply global filters
    
    #complete checklists only
    complete_filter <- if(input$complete_checklist_filter == "Yes") 1 else c(0, 1)
    
    x <- user_data_raw() |> 
      filter(all_obs_reported %in% complete_filter)
    
    #year slider
    x <- x |> 
      filter(between(obs_date_y, input$year_slider[1], input$year_slider[2]))
    
    x
    
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
  
  #checklists
  
  checklist_graph_data <- reactive({
    
    checklist_data() |> 
      distinct(submission_id, !!input$checklist_date_selector) |> 
      count(!!input$checklist_date_selector) |> 
      mutate(n_cumsum = cumsum(n))
    
  })
  
  output$obs_linechart <- renderPlot({
    
    req(input$checklist_date_selector)
    
    checklist_rows <- nrow(checklist_graph_data())
    
    last_checklist <- checklist_graph_data() |> slice(checklist_rows)
    
    total_checklists <- last_checklist |> pull(n_cumsum)
    
    checklist_graph_data() |> 
      ggplot(aes(!!input$checklist_date_selector, n_cumsum)) +
      geom_line() +
      geom_point(data = last_checklist, 
                 aes(x = !!input$checklist_date_selector, y = total_checklists)) +
      geom_label(data = last_checklist, 
                 aes(x = !!input$checklist_date_selector, y = total_checklists, label = total_checklists),
                 nudge_x = 75) +
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
  
  #lifers
  
  lifer_df <- reactive({
    
    user_data() |> 
      anti_join(species_exclude_df, by = join_by(common_name)) |> #not working
      filter(!str_detect(common_name, "sp.")) |> 
      filter(!str_detect(common_name, "\\/")) |> 
      mutate(common_name = str_remove(common_name, "\\([^()]+\\)")) |> 
      mutate(common_name = str_squish(common_name)) |> 
      select(obs_date, common_name) |> 
      distinct() |> 
      arrange(obs_date) |> 
      group_by(common_name) |> 
      mutate(common_name_cumsum = dense_rank(obs_date)) |> 
      mutate(is_lifer = common_name_cumsum == 1) |> 
      filter(is_lifer == TRUE) |> 
      ungroup() |> 
      mutate(lifer_cumsum = row_number())
    
  })
  
  output$lifer_linechart <- renderPlot({
    
    lifer_rows <- nrow(lifer_df())
    
    last_lifer <- lifer_df() |> slice(lifer_rows)
    
    total_lifers <- last_lifer |> pull(lifer_cumsum)
    
    lifer_df() |> 
      ggplot(aes(obs_date, lifer_cumsum)) +
      geom_line() +
      geom_point(data = last_lifer,
                 size = 2) +
      geom_label(data = last_lifer,
                 aes(label = total_lifers),
                 nudge_x = 70)
    
  })
  
  output$lifer_table <- renderReactable({
    
    lifer_df() |> 
      select(obs_date, common_name, lifer_cumsum) |> 
      reactable(columns = list(
        
        obs_date = colDef(name = "Observation Date",
                          filterable = TRUE),
        common_name = colDef(name = "Species (Common name)",
                             filterable = TRUE),
        lifer_cumsum = colDef(name = "Lifer #")
        
      ))
    
  })
  
  #species detection
  
  species_detection_df <- reactive({
    
    user_data() |> 
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
  
  #trigger modal dialog if axes are the same
  
  observeEvent(c(input$effort_axis_x, input$effort_axis_y), {
    
    if((input$effort_axis_x == input$effort_axis_y) && isTruthy(input$effort_axis_x) && isTruthy(input$effort_axis_y)) {
      showModal(modalDialog(
        title = "Oops!",
        "Choose different columns for each axis",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
  })
  
  output$effort_vs_species_count <- renderPlot({
    
    req(input$effort_axis_x != input$effort_axis_y)
    
    species_detection_df() |> 
      filter(flag_travelling == TRUE) |> 
      ggplot(aes(!!input$effort_axis_x, !!input$effort_axis_y, size = `Species detected`)) +
      geom_jitter(alpha = .3) +
      labs(title = "Checklist effort vs species detected",
           x = input$effort_axis_x,
           y = input$effort_axis_y,
           size = "Species detected")
    
  })
  
}