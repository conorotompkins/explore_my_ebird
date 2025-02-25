library(shiny)
library(bslib)
library(tidyverse)
library(reactable)

date_df <- tribble(~Year, ~`Year-month`, ~`Year-week`, ~Date,
                   NA, NA, NA, NA)

heatmap_axis_var_df <- tribble(~Year, ~Month, ~Week, ~Weekday, ~Hour,
                               NA, NA, NA, NA, NA)

heatmap_metric_var_df <- tribble(~`Checklist count`, ~`Species count`,
                                 NA, NA)

species_detection_axis_vars_df <- tribble(~`Distance traveled`, ~Duration,
                                          NA, NA)

# Define UI
page_navbar(
  
  title = "Explore your eBird data",
  
  sidebar = sidebar(
    
    title = "Global filters",
    
    width = 310,
    
    radioButtons(inputId = "complete_checklist_filter",
                 label = "Only include complete checklists",
                 choices = c("Yes", "No"),
                 selected = "Yes"),
    
    sliderInput(inputId = "year_slider",
                label = "Choose time period",
                min = 2019, max = year(Sys.Date()), value = c(2019, year(Sys.Date())),
                step = 1, sep = "")
    
  ),
  
  nav_item(
    
    actionButton(inputId = "trigger_upload_modal",
                 label = "Upload eBird CSV")
    
  ),
  
  nav_panel(
    
    "Checklists",
    
    navset_card_tab(
      
      nav_panel(
        
        title = "Line graph",
        
        # Sidebar with a slider input for number of bins
        layout_sidebar(
          
          sidebar = sidebar(
            
            varSelectizeInput(inputId = "checklist_date_selector",
                              label = "Select timespan",
                              data = date_df,
                              selected = "Date",
                              multiple = FALSE)
            
          ),
          
          plotOutput("obs_linechart")
          
        )
      ),
      
      nav_panel(
        
        title = "Heatmap",
        
        layout_sidebar(
          
          sidebar = sidebar(
            
            varSelectizeInput(inputId = "checklist_metric_selector",
                              label = "Select metric",
                              data = heatmap_metric_var_df,
                              multiple = FALSE),
            
            varSelectizeInput(inputId = "checklist_date_selector_x",
                              label = "Select X axis",
                              data = heatmap_axis_var_df,
                              selected = "Month",
                              multiple = FALSE),
            
            varSelectizeInput(inputId = "checklist_date_selector_y",
                              label = "Select Y axis",
                              data = heatmap_axis_var_df,
                              selected = "Year",
                              multiple = FALSE)
            
          ),
          
          plotOutput("checklist_heatmap")
          
        )
      )
    )
  ),
  
  nav_panel(
    
    title = "Lifers",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        title = "Placeholder sidebar"
        
      ),
      
      navset_card_tab(
        
        nav_panel(
          
          title = "Line chart",
          
          plotOutput("lifer_linechart")
          
        ),
        
        nav_panel(
          
          "Table",
          
          reactableOutput("lifer_table")
          
        )
      )
    )
  ),
  
  nav_panel(
    
    title = "Species detection",
    
    navset_card_tab(
      
      nav_panel(
        
        "Effort",
        
        layout_sidebar(
          
          sidebar = sidebar(
            
            title = "Select chart variables",
            
            varSelectizeInput(inputId = "effort_axis_x",
                              label = "Select X axis",
                              data = species_detection_axis_vars_df,
                              selected = "Distance traveled"),
            
            varSelectizeInput(inputId = "effort_axis_y",
                              label = "Select Y axis",
                              data = species_detection_axis_vars_df,
                              selected = "Duration")
            
          ),
          
          plotOutput("effort_vs_species_count")
          
        )
      )
    )
  )
)