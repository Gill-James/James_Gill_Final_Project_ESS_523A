# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#set up for the shiny app
library(tmap)
library(sf)
library(dplyr)
library(shiny)
library(plotly)

# read in data
load("finalprojdata2.RData")

# set tmap mode to interactive
tmap_mode("view")

ui <- fluidPage(
  #App title
  titlePanel("Invasive Plant Species in Colorado by Land Cover"),
  
  # Description
  h5(
    "This app provides information about invasive plant occurrences in Colorado. The map below displays occurrence locations for Cheatgrass (Bromus tectorum L.), Chinese Saltcedar (Tamarix chinensis), and Yellow Toadflax (Linaria vulgaris). The occurrences can be filtered by species, observation type, land cover class, and year of observation. The bar chart below vizualizes species occurences by land cover."
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel 
    sidebarPanel(
      # Input: species on map
      checkboxGroupInput(
        inputId = "species",
        label = "Species",
        choices = list("Cheatgrass", "Chinese Saltcedar", "Yellow Toadflax"),
        selected = c("Cheatgrass", "Chinese Saltcedar", "Yellow Toadflax")
      ),
      
      # Input: Filter points by observation type
      checkboxGroupInput(
        inputId = "obs",
        label = "Observation Type",
        choiceNames = list(
          "Human Observation",
          "Preserved Specimen",
          "Machine Observation"
        ),
        choiceValues = list(
          "HUMAN_OBSERVATION",
          "PRESERVED_SPECIMEN",
          "MACHINE_OBSERVATION"
        ),
        selected = c("HUMAN_OBSERVATION",
                     "PRESERVED_SPECIMEN",
                     "MACHINE_OBSERVATION"
        )
      ),
      # Input: Filter by land cover
      checkboxGroupInput(
        inputId = "NLCD.Land.Cover.Class",
        label = "Land Cover Class",
        choiceNames = list(
          "Cultivated Crops",
          "Developed, Low Intensity",
          "Developed, Medium Intensity",
          "Evergreen Forest",
          "Herbaceous",
          "Shrub/Scrub"
        ),
        choiceValues = list(
          "Cultivated Crops",
          "Developed, Low Intensity",
          "Developed, Medium Intensity",
          "Evergreen Forest",
          "Herbaceous",
          "Shrub/Scrub"
        ),
        selected = c("Cultivated Crops",
                     "Developed, Low Intensity",
                     "Developed, Medium Intensity",
                     "Evergreen Forest",
                     "Herbaceous",
                     "Shrub/Scrub"
        )
      ),
      # Input: Filter by year
      sliderInput(
        inputId = "year",
        label = "Observation Year",
        min = 1895,
        max = 2022,
        value = c(1895, 2022)
      )
    ),
    
    # Main panel to display map
    mainPanel(
      tags$div(
        style = "margin-bottom: 20px;",
      # Output: interactive tmap object
      tmapOutput("map"),
      ),
      
      #Output: bar chart
      plotlyOutput("plot"),
      
    )
  )
)

# Define server logic 
server <- function(input, output){
  
  # Make a reactive observation object for the map
  combined_sf_react <- reactive({
    combined_sf %>%
      filter(Species %in% input$species) %>%
      filter(basisOfRecord %in% input$obs)%>%
      filter(NLCD.Land.Cover.Class %in% input$NLCD.Land.Cover.Class)%>%
      filter(year >= input$year[1] & year <= input$year[2])
  })
  
  # Make species_by_cover_react
  species_by_cover_react <- species_by_cover
  
  # Render map
  output$map <- renderTmap({
    tm_shape(combined_sf_react()) +
    tm_dots(
      col = "Species",
      size = 0.1,
      palette = "viridis",
      title = "Invasive Plant Species Observations",
      popup.vars = c(
        "Species" = "Species",
        "Record Type" = "basisOfRecord",
        "Land Cover Type" = "NLCD.Land.Cover.Class",
        "County" = "NAME",
        "Year" = "year",
        "Month" = "month"
      )
    ) +
      tm_shape(co_nlcd_counties_sf)+
      tm_borders(col = "black", lwd = 0.3)
    
  })
  
  # Render bar chart
  output$plot <- renderPlotly({plot_ly(species_by_cover_react, x = ~NLCD.Land.Cover.Class) %>%
    add_trace(y = ~cheatgrass_total, name = "Cheatgrass", type = "bar", marker = list(color = "#440154")) %>%
    add_trace(y = ~toadflax_total, name = "Yellow Toadflax", type = "bar", marker = list(color = "#FDE725")) %>%
    add_trace(y = ~saltcedar_total, name = "Chinese Saltcedar", type = "bar", marker = list(color = "#21908C")) %>%
    layout(title = "Colorado Invasive Plant Occurrences by Land Cover",
           xaxis = list(title = "Land Cover Type"),
           yaxis = list(title = "Total Occurrences"))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)