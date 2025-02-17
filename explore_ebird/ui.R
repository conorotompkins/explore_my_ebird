library(shiny)
library(bslib)

# Define UI
page_navbar(
  
  title = "Explore your eBird data",
  
  layout_sidebar(
    
    sidebar = sidebar(
      
      width = 310,
      
      fileInput(inputId = "upload",
                label = "Upload eBird CSV",
                accept = ".csv")
      
    ),
    
    navset_card_tab(
      
      nav_panel(
        
        title = "Checklists",
        
        navset_card_tab(
          
          id = "tab_nav_2",
          
          nav_panel(
            
            title = "Line graph",
            
            # Sidebar with a slider input for number of bins
            layout_sidebar(
              
              sidebar = sidebar(
                
                varSelectizeInput(inputId = "checklist_date_selector",
                                  label = "Select timespan",
                                  data = NULL,
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
      )
    )
  )
)