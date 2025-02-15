library(shiny)
library(bslib)

# Define UI
page_navbar(
  
  # Application title
  title = "Explore your eBird data",
  
  navset_card_tab(
    
    nav_panel(
      
      title = "Checklists",
      
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
      
      title = "Lifers" 
      
    )
  )
)