library(shiny)
library(tidyverse)
library(scales)
library(tsibble)
library(janitor)
library(here)
library(reactable)
library(broom)
library(tune)
library(sf)
library(mapgl)
library(glue)

options(scipen = 999, digits = 4)

theme_set(theme_bw(base_size = 18))

species_exclude_df <- tibble(
  common_name = c("Muscovy Duck (Domestic type)")
)

# Define server logic required to draw a histogram
function(input, output, session) {
  #trigger modal to upload user data
  observeEvent(input$trigger_upload_modal, {
    showModal(
      modalDialog(
        title = "Upload eBird data!",

        renderUI(
          fileInput(
            inputId = "upload",
            label = "Upload eBird CSV",
            accept = ".csv"
          )
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
      rename(obs_date = date, species_count = count) |>
      mutate(
        obs_date_time = str_c(obs_date, time) |> ymd_hms(),
        obs_date_ym = yearmonth(obs_date),
        obs_date_yw = yearweek(obs_date),
        obs_date_y = year(obs_date),
        obs_date_m = month(obs_date, label = TRUE, abbr = TRUE) |>
          as.character() |>
          factor(levels = month.abb),
        obs_date_w = isoweek(obs_date),
        obs_date_wday = wday(obs_date, label = TRUE, abbr = TRUE),
        obs_date_hour = hour(time)
      ) |>
      arrange(submission_id)
  })

  top_state_province_county <- reactive({
    user_data_raw() |>
      count(state_province, county, sort = TRUE)
  })

  observeEvent(user_data_raw(), {
    options <- top_state_province_county() |>
      pull(state_province)

    updateSelectizeInput(
      inputId = "user_state_province",
      choices = options,
      selected = options[1]
    )
  })

  observeEvent(user_data_raw(), {
    options <- top_state_province_county() |>
      pull(county)

    updateSelectizeInput(
      inputId = "user_county",
      choices = options,
      selected = options[1]
    )
  })

  observeEvent(input$user_state_province, {
    options <- top_state_province_county() |>
      filter(state_province == input$user_state_province) |>
      pull(county)

    updateSelectizeInput(
      inputId = "user_county",
      choices = options,
      selected = options[1]
    )
  })

  observeEvent(user_data_raw(), {
    options <- user_data_raw() |>
      count(location, sort = TRUE) |>
      pull(location)

    updateSelectizeInput(
      inputId = "user_patch",
      choices = options,
      selected = options[1]
    )
  })

  user_data <- reactive({
    #apply global filters

    #complete checklists only
    complete_filter <- if (input$complete_checklist_filter == "Yes") 1 else c(0, 1)

    x <- user_data_raw() |>
      filter(all_obs_reported %in% complete_filter)

    #year slider
    x <- x |>
      filter(between(obs_date_y, input$year_slider[1], input$year_slider[2]))

    x
  })

  checklist_data <- reactive({
    user_data() |>
      select(
        submission_id,
        location,
        common_name,
        duration_min,
        obs_date,
        obs_date_y,
        obs_date_ym,
        obs_date_yw,
        obs_date_m,
        obs_date_w,
        obs_date_wday,
        obs_date_hour,
        latitude,
        longitude
      ) |>
      rename(
        Date = obs_date,
        Year = obs_date_y,
        `Year-month` = obs_date_ym,
        `Year-week` = obs_date_yw,
        Month = obs_date_m,
        Week = obs_date_w,
        Weekday = obs_date_wday,
        Hour = obs_date_hour
      ) |>
      arrange(submission_id) |>
      group_by(
        submission_id,
        location,
        Date,
        Year,
        `Year-month`,
        `Year-week`,
        Month,
        Week,
        Weekday,
        Hour,
        latitude,
        longitude
      ) |>
      summarize(
        species_count = n_distinct(common_name),
        duration = sum(duration_min)
      ) |>
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
      geom_line(lwd = 1) +
      geom_point(
        data = last_checklist,
        aes(x = !!input$checklist_date_selector, y = total_checklists)
      ) +
      geom_label(
        data = last_checklist,
        aes(
          x = !!input$checklist_date_selector,
          y = total_checklists,
          label = comma(total_checklists)
        ),
        size = 6
      ) +
      scale_y_continuous(expand = expansion(c(.01, .1))) +
      labs(title = "Checklist count over time", y = "Checklists") +
      theme(panel.grid.minor = element_blank())
  })

  heatmap_date_time_count <- reactive({
    checklist_data() |>
      select(
        submission_id,
        !!input$checklist_date_selector_x,
        !!input$checklist_date_selector_y,
        species_count
      ) |>
      group_by(
        !!input$checklist_date_selector_x,
        !!input$checklist_date_selector_y
      ) |>
      summarize(
        checklist_count = n_distinct(submission_id),
        species_count_mean = mean(species_count),
        species_count_max = max(species_count)
      ) |>
      ungroup() |>
      complete(
        !!input$checklist_date_selector_x,
        !!input$checklist_date_selector_y
      ) |>
      replace_na(
        list(
          checklist_count = 0,
          species_count_mean = 0,
          species_count_max = 0
        )
      ) |>
      rename(
        `Checklist count` = checklist_count,
        `Species count (avg)` = species_count_mean,
        `Species count (max)` = species_count_max
      )
  })

  heatmap_cols <- reactive({
    checklist_data() |>
      select(Year, Month, Week, Weekday, Hour)
  })

  observeEvent(checklist_data(), {
    var_cols <- heatmap_date_time_count() |>
      select(`Checklist count`, contains("Species count"))

    updateVarSelectizeInput(
      inputId = "checklist_metric_selector",
      data = var_cols,
      selected = "Checklist count"
    )
  })

  #trigger modal dialog if axes are the same

  observeEvent(
    c(input$checklist_date_selector_x, input$checklist_date_selector_y),
    {
      if (
        (input$checklist_date_selector_x == input$checklist_date_selector_y) &&
          isTruthy(input$checklist_date_selector_x) &&
          isTruthy(input$checklist_date_selector_y)
      ) {
        showModal(modalDialog(
          title = "Oops!",
          "Choose different columns for each axis",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  )

  output$checklist_heatmap <- renderPlot({
    req(
      input$checklist_date_selector_x != input$checklist_date_selector_y,
      input$checklist_metric_selector
    )

    x_axis_type <- if (input$checklist_date_selector_x == "Month" || input$checklist_date_selector_x == "Weekday") "discrete" else "continuous"

    y_axis_type <- if (input$checklist_date_selector_y == "Month" || input$checklist_date_selector_y == "Weekday") "discrete" else "continuous"

    scale_x <- function(x) {
      switch(x_axis_type, discrete = scale_x_discrete(expand = c(0, 0)), continuous = scale_x_continuous(expand = c(0, 0)))
    }

    scale_y <- function(x) {
      switch(y_axis_type, discrete = scale_y_discrete(expand = c(0, 0)), continuous = scale_y_continuous(expand = c(0, 0)))
    }

    heatmap_date_time_count() |>
      ggplot(aes(
        x = !!input$checklist_date_selector_x,
        y = !!input$checklist_date_selector_y,
        fill = !!input$checklist_metric_selector
      )) +
      geom_tile() +
      scale_fill_viridis_c() +
      scale_x() +
      scale_y() +
      labs(title = "Checklist heatmap", fill = input$checklist_metric_selector)
  })

  output$checklist_table <- renderReactable({
    checklist_data() |>
      select(submission_id, Date, location, species_count, duration) |>
      arrange(desc(Date)) |>
      reactable()
  })

  output$checklist_map <- renderMaplibre({
    checklist_map <- checklist_data() |>
      summarize(species_count_mean = mean(species_count), species_count_max = max(species_count), checklist_count = n(), .by = c(location, longitude, latitude))

    checklist_map$popup <- glue(
      "<strong>Location: </strong>{checklist_map$location}<br><strong>Maximum species count: </strong>{checklist_map$species_count_max} <br><strong>Checklists: </strong> {checklist_map$checklist_count}"
    )

    checklist_map$tooltip <- glue(
      "{checklist_map$location}"
    )

    checklist_map <- checklist_map |>
      st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

    continuous_scale <- interpolate_palette(
      data = checklist_map,
      column = "species_count_max",
      method = "equal",
      n = 5,
      palette = viridisLite::plasma
    )

    maplibre(bounds = checklist_map) |>
      add_circle_layer(source = checklist_map, id = "checklist_circles", circle_color = continuous_scale$expression, popup = "popup", tooltip = "tooltip") |>
      add_legend(
        "Maximum species detected",
        values = get_legend_labels(continuous_scale, digits = 0),
        colors = get_legend_colors(continuous_scale),
        type = "continuous"
      )
  })

  #lifers

  lifer_df <- reactive({
    x <- user_data() |>
      filter(!str_detect(common_name, "sp.")) |>
      filter(!str_detect(common_name, "\\/")) |>
      anti_join(species_exclude_df) |>
      mutate(common_name = str_remove(common_name, "\\([^()]+\\)")) |>
      mutate(common_name = str_squish(common_name)) |>
      select(submission_id, obs_date_time, common_name) |>
      arrange(common_name, obs_date_time) |>
      group_by(common_name) |>
      slice_head(n = 1) |>
      ungroup() |>
      arrange(obs_date_time) |>
      mutate(lifer_cumsum = row_number()) |>
      arrange(desc(lifer_cumsum)) |>
      mutate(obs_date = as_date(obs_date_time)) |>
      select(-obs_date_time)

    x |>
      left_join(user_data() |> distinct(submission_id, state_province, county, location)) |>
      select(lifer_cumsum, common_name, obs_date, state_province, county, location, submission_id)
  })

  output$lifer_linechart <- renderPlot({
    last_lifer <- lifer_df() |>
      slice(1) |>
      mutate(description = glue("Lifer #{lifer_cumsum}: {common_name}"))

    total_lifers <- last_lifer |> pull(lifer_cumsum)

    print(lifer_df())

    lifer_df() |>
      ggplot(aes(obs_date, lifer_cumsum)) +
      geom_path() +
      geom_label(
        data = last_lifer,
        aes(label = description),
        size = 4,
        nudge_x = 120
      ) +
      scale_x_date(expand = expansion(mult = c(0, .1))) +
      labs(
        title = "Lifer species over time",
        x = "Observation date",
        y = "Lifer #"
      )
  })

  output$lifer_table <- renderReactable({
    lifer_df() |>
      select(
        lifer_cumsum,
        common_name,
        obs_date,
        state_province,
        county,
        location,
        submission_id
      ) |>
      reactable(
        columns = list(
          lifer_cumsum = colDef(name = "Lifer #"),
          obs_date = colDef(name = "Observation Date", filterable = TRUE),
          common_name = colDef(name = "Species (Common name)", filterable = TRUE),
          state_province = colDef(name = "State/Province", filterable = TRUE),
          county = colDef(name = "County", filterable = TRUE),
          location = colDef(name = "Location", filterable = TRUE),
          submission_id = colDef(name = "Checklist")
        )
      )
  })

  #species detection

  species_detection_df <- reactive({
    user_data() |>
      filter(all_obs_reported == 1) |>
      select(
        submission_id,
        protocol,
        state_province,
        location,
        starts_with("obs_date"),
        number_of_observers,
        duration_min,
        distance_traveled_km,
        common_name
      ) |>
      group_by(
        submission_id,
        protocol,
        state_province,
        location,
        obs_date_m,
        obs_date_wday,
        obs_date_hour,
        number_of_observers,
        duration_min,
        distance_traveled_km
      ) |>
      summarize(species_count = n_distinct(common_name)) |>
      ungroup() |>
      mutate(flag_travelling = !is.na(distance_traveled_km)) |>
      replace_na(list(distance_traveled_km = 0))
  })

  #trigger modal dialog if axes are the same

  observeEvent(c(input$effort_axis_x, input$effort_axis_y), {
    if (
      (input$effort_axis_x == input$effort_axis_y) &&
        isTruthy(input$effort_axis_x) &&
        isTruthy(input$effort_axis_y)
    ) {
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
      rename(
        `Distance traveled` = distance_traveled_km,
        Duration = duration_min,
        `Species detected` = species_count
      ) |>
      ggplot(aes(
        !!input$effort_axis_x,
        !!input$effort_axis_y,
        size = `Species detected`
      )) +
      geom_jitter(alpha = .3) +
      labs(
        title = "Checklist effort vs species detected",
        x = input$effort_axis_x,
        y = input$effort_axis_y,
        size = "Species detected"
      )
  })

  model_df <- reactive({
    species_detection_df() |>
      mutate(flag_my_patch = location == input$user_patch) |>
      mutate(cumulative_checklist_no = dense_rank(submission_id)) |>
      mutate(
        hour_block = case_when(
          between(obs_date_hour, 0, 3) ~ "12am-3am",
          between(obs_date_hour, 4, 7) ~ "4am-7am",
          between(obs_date_hour, 8, 11) ~ "8am-11am",
          between(obs_date_hour, 12, 15) ~ "12pm-3pm",
          between(obs_date_hour, 16, 19) ~ "4pm-7pm",
          between(obs_date_hour, 20, 23) ~ "8pm-11pm"
        ),
        hour_block = factor(
          hour_block,
          levels = c(
            "12am-3am",
            "4am-7am",
            "8am-11am",
            "12pm-3pm",
            "4pm-7pm",
            "8pm-11pm"
          )
        )
      ) |>
      mutate(flag_my_state = state_province == input$user_state_province) |>
      mutate(species_count_log10 = log10(species_count))
  })

  model <- reactive({
    data_train <- slice_sample(
      model_df(),
      prop = .8,
      by = species_count_log10
    )

    data_test <- anti_join(
      model_df(),
      data_train,
      by = join_by(submission_id)
    )

    lm(
      species_count ~ log(distance_traveled_km + 1) * duration_min + obs_date_m + hour_block + flag_my_patch + cumulative_checklist_no + flag_my_state + log(number_of_observers) + protocol,
      data = data_train
    )
  })

  model_terms <- reactive({
    model() |>
      tidy() |>
      arrange(desc(abs(estimate))) |>
      select(term, estimate, p.value)
  })

  output$model_terms_table <- renderReactable({
    model_terms() |>
      reactable(
        columns = list(
          term = colDef("Variable"),
          estimate = colDef("Estimate", format = colFormat(digits = 1)),
          p.value = colDef("p value", format = colFormat(digits = 2))
        )
      )
  })

  model_predictions <- reactive({
    preds <- predict(model(), model_df())

    model_df() |>
      mutate(.pred = preds, .resid = species_count - .pred)
  })

  output$model_prediction_graph <- renderPlot(
    {
      model_predictions() |>
        ggplot(aes(species_count, .pred)) +
        geom_abline() +
        geom_jitter(alpha = .3) +
        geom_smooth() +
        coord_obs_pred() +
        labs(title = "Actual vs. predicted species count per checklist", x = "Actual species count", y = "Prediction") +
        theme(plot.title = element_text(size = 14))
    },
    res = 96
  )

  output$model_terms_graph <- renderPlot({
    model_terms() |>
      mutate(term = fct_reorder(term, abs(estimate)), direction = ifelse(estimate > 0, "Positive", "Negative")) |>
      ggplot(aes(estimate, term, fill = direction)) +
      geom_vline(xintercept = 0) +
      geom_col(color = "black") +
      scale_fill_viridis_d() +
      labs(title = "Model terms", x = "Coefficient", y = NULL) +
      guides(fill = "none") +
      theme(panel.grid.minor = element_blank())
  })
}
