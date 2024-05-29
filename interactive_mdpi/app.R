#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(sf)

data_path <- readRDS("factor_scores_geoids.rds")

# Define UI for application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_factor", "Select Factor:", 
                  choices = c(
                    #Names simplified, also for completeness
                              "Poverty Rate" = "poverty_rate", 
                              "Education and Health" = "Education and Health", 
                              "Employment and Shelter" = "Employment and Shelter", 
                              "Labor and Housing Cost" = "Labor and Housing Cost", 
                              "The Digital Divide" = "The Digital Divide", 
                              "School Attendance" = "School Attendance"))
    ),
    mainPanel(
      leafletOutput("interactiveMap", height = "80vh"),
      # Added a note at the bottom of the main panel to help make reading cloropleth map more intuitive
      tags$hr(),
      tags$p("Note: Higher numbers indicate greater levels of deprivation. Darker colors on the map correspond to areas with higher deprivation scores.")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$interactiveMap <- renderLeaflet({
    leaflet(data = data_path) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -87.8, lat = 41.8, zoom = 9) %>%
      addPolygons(
        fillColor = ~colorQuantile(palette = "YlOrRd", domain = data_path[[input$selected_factor]], n = 10)(data_path[[input$selected_factor]]),
        weight = 1,
        color = "#BDBDC3",
        fillOpacity = 0.7,
        popup = ~paste("Community: ", community_name,
                       "<br>GeoID: ", GEOID,
                       "<br>Score: ", round(data_path[[input$selected_factor]][which(data_path$GEOID == GEOID)], 2),
                       "<br>Average Neighbor Score: ", round(data_path[[paste0(input$selected_factor, "_neighbor_average")]][which(data_path$GEOID == GEOID)], 2))
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
