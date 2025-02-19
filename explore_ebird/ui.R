library(shiny)
library(bslib)

date_df <- tribble(~Year, ~`Year-month`, ~`Year-week`, ~Date,
                   NA, NA, NA, NA)

# Define UI
page_navbar(
  
  title = "Explore your eBird data",
  
  sidebar = sidebar(
    
    title = "Global filters",
    
    width = 310,
    
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
                              data = NULL,
                              multiple = FALSE),
            
            varSelectizeInput(inputId = "checklist_date_selector_x",
                              label = "Select X axis",
                              data = NULL,
                              multiple = FALSE),
            
            varSelectizeInput(inputId = "checklist_date_selector_y",
                              label = "Select Y axis",
                              data = NULL,
                              multiple = FALSE)
            
          ),
          
          plotOutput("checklist_heatmap")
          
        )
      )
    )
  ),
  
  nav_panel(
    
    title = "Lifers",
    
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
                              data = NULL,
                              selected = NULL),
            
            varSelectizeInput(inputId = "effort_axis_y",
                              label = "Select Y axis",
                              data = NULL,
                              selected = NULL)
            
          ),
          
          plotOutput("effort_vs_species_count")
          
        )
      )
    )
  )
)