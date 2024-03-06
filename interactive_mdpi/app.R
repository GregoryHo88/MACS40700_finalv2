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
library(sf) #This is the culprit that made me spent 1++ hours troubleshooting

data_path <- readRDS("factor_scores_geoids.rds")

# Define UI for application
ui <- fluidPage(
  titlePanel("Interactive Map of Deprivation Factors"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_factor", "Select Factor:", 
                  choices = c(
                    #Names simplified 
                              "Poverty Rate" = "poverty_rate", 
                              "Educational and Health Deprivation" = "Education and Health", 
                              "Employment and Housing Condition" = "Employment and Shelter", 
                              "Labor Force Participation and Housing Affordability" = "Labor and Housing Cost", 
                              "The Digital Divide" = "The Digital Divide", 
                              "School Attendance" = "School Attendance"))
    ),
    mainPanel(
      leafletOutput("interactiveMap", height = "80vh")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$interactiveMap <- renderLeaflet({
    # Create the leaflet map and bind data_path as the data source
    leafletMap <- leaflet(data = data_path) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -87.8, lat = 41.8, zoom = 9)
    
    # Dynamically determine the fillColor based on the selected factor
    colorPal <- colorBin(palette = "YlOrRd", domain = data_path[[input$selected_factor]], bins = 10)
    fillColor <- reactive({ colorPal(data_path[[input$selected_factor]]) })
    
    # Observe changes in input$selected_factor and update the polygons
    observe({
      leafletProxy("interactiveMap", data = data_path) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~fillColor(), 
                    weight = 1,
                    color = "#BDBDC3",
                    fillOpacity = 0.8,
                    popup = ~paste("GeoID: ", GEOID,
                                   "<br>Score: ", round(get(input$selected_factor), 2),
                                   "<br>Average Neighbor Score: ", round(get(paste0(input$selected_factor, "_neighbor_average")), 2)))
    })
    
    return(leafletMap)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
