# Packages
library(sf)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(tidycensus)
library(rgdal)

# Environment Settings
#setwd("D:/ajax/trusty_analytics/SA/PDX_HousingExplorer/PDX_Housing_Dashboard/")

# Read in Data
pdx_sf <- readOGR("Data/pdx_ee_clean_1172019/pdx_ee_clean_1172019.shp") # read in data
projPdx<-spTransform(pdx_sf,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # set CRS


# Define UI for application that plots features of movies
ui <- dashboardPage(
  
  dashboardHeader(title = "Portland Housing Characteristics Data Explorer", titleWidth = 500),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 4,
             box(width = NULL, 
                 selectInput("mapvar", 
                             label = "Select a variable to map: ",
                             choices = names(pdx_sf),
                             selected = "bldgsqft")
             ),
             box(width = NULL,
                 plotOutput(outputId = "histogram", height = 380)
             ),
             box(width = NULL,
                 selectInput(inputId = "y",
                             label = "Y-axis:",
                             choices = names(pdx_sf),
                             selected = "MHI"),
                 
                 # Select variable for x-axis
                 selectInput(inputId = "x", 
                             label = "X-axis:",
                             choices = names(pdx_sf),
                             selected = "bldgsqft")
             )
      ),
      column(width = 8,
        box(width = NULL, solidHeader = TRUE,
            leafletOutput(outputId = "map", height = 500)
        ),
        box(width = NULL, 
            plotOutput(outputId = "scatterplot", height = 350)
        )
      )
    )
  ), skin = "red")

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = projPdx@data[,input$mapvar])
    
    
    # Create a popup
    popup <- paste0("<strong>Mean Building Size (sq.ft): </strong>", 
                    round(projPdx$bldgsqft, 2), 
                    "<br><strong>Median Year Built: </strong>", 
                    round(projPdx$MEDYRBUILT, 2),
                    "<br><strong>Mean Lot Size (Acres): </strong>", 
                    round(projPdx$gis_acres, 2), 
                    "<br><strong>Mean Building Value: </strong>", 
                    round(projPdx$bldgval, 2),
                    "<br><strong>Housing Density (Buildings / Acre): </strong>", 
                    round(projPdx$density, 4), 
                    "<br><strong>Median Household Income: </strong>", 
                    round(projPdx$MHI, 2),
                    "<br><strong>Population with Bachelor's or Higher: </strong>", 
                    round(projPdx$GT25_GTBA, 4), 
                    "<br><strong>% Renter Population: </strong>", 
                    round(projPdx$PCTRENT, 2),
                    "<br><strong>Mean Household Size: </strong>", 
                    round(projPdx$AVG_HHSIZE, 2))
    
    leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(data = projPdx, weight = 1, fillOpacity = 0.75,
                  fillColor = ~pal(projPdx@data[,input$mapvar]), color = "#BDBDC3", popup = popup) %>% 
      addLegend(position = "bottomright", pal = pal, values = projPdx@data[,input$mapvar],
                title = paste(as.character(input$mapvar)),
                opacity = 1)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = as.data.frame(pdx_sf), aes_string(x = input$x, y = input$y)) +
      geom_point(color = "orangered2")
  })
  
  # Create histogram
  output$histogram <- renderPlot({
    ggplot(projPdx@data, aes(x = projPdx@data[,input$mapvar])) +
      geom_histogram(color = "black", fill = "orangered2") +
      xlab(paste(input$mapvar))
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)