setwd("/Users/allymuszynski/Desktop/GIS3FIN")

library(spData)
library(sf)
library(sp)
library(ggmap)
library(dplyr)
library(maptools)
library(ggplot2)
library(doBy)
library(shinydashboard)
library(png)
library(shinydashboardPlus)
library(shiny)
library(tidyverse)
library(magrittr)
library(highcharter)
library(shinyjs)
library(tmaptools)
library(leaflet)
library(mapdeck)
library(shiny)
library(rgdal)
library(rgeos)
library(tmap)
library(vioplot)
library(sm)


# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Miami Sea Level Rise Explorer"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            h3("Make your choice"),
            helpText("Create an interactive map of Miami Sea Level Rise."),
            selectInput("Miami_shpv_sf", 
                        label = "Choose a variable to display",
                        choices = list("Percent White", 
                                       "Public Assistance Per Capita",
                                       "Per Capita Income", 
                                       "Value of Owner-occupied Housing Units"),
                        selected = "Public Assistance Per Capita"),
            sliderInput("integer", "Sea Level Rise in Feet:",
                        min = 1, max = 6,
                        value = 1), 
            h4("About"),
            p("This app works to show how sea level rise affects varied communities in Miami-Dade County by % white per tract, public assistance rate, per capita income, and value of houning units occupied by tenents. It shows which communities will be the most affected by SLR and what precautions must be taken to ensure minimal losses."),
            h4("Data accessed from IPUMS NHGIS ACS 5-Year Survey from 2015-2019 and Miami-Dade Open Data Portal"),
            p("App by Ally Muszynski, 2021")
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            textOutput("Miami_shpv_sf"),
            textOutput("min_max"),
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Map", leafletOutput("working_map"))
                        
            )
            
        )
    )
)
# Define server logic for random distribution app ----
server <- function(input, output) {

    output$Miami_shpv_sf <- renderText({
        paste("You have selected", input$Miami_shpv_sf)
    })

    MiamiData <- st_read("/Users/allymuszynski/Desktop/GIS3FIN/Miami_2/MiamiClean.shp")

    output$working_map <- renderLeaflet({
        data <- switch(input$Miami_shpv_sf,
                       "Percent White" = "RWtePerCap",
                       "Public Assistance Per Capita" = "PblcAsstPC",
                       "Per Capita Income" = "PCI",
                       "Value of Owner-occupied Housing Units" = "HseValIndx")

        color <- switch(input$Miami_shpv_sf,
                        "Percent White" = "darkorange",
                        "Public Assistance Per Capita" = "darkorange",
                        "Per Capita Income" = "darkorange",
                        "Value of Owner-occupied Housing Units" = "darkorange")

        legend <- switch(input$Miami_shpv_sf,
                         "Percent White" = "% White",
                         "Public Assistance Per Capita" = "% Public Assistance Per Capita",
                         "Per Capita Income" = "Per Capita Income",
                         "Value of Owner-occupied Housing Units" = "% Value of Owner-occupied Housing Units")



        working_map <- tm_shape(Miami_shpv) + tm_fill(data, title=input$Miami_shpv_sf, style="jenks")
        tmap_leaflet(working_map)
    })
    output$min_max <- renderText({
        paste("You have chosen a range that goes from",
              input$range[1], "to", input$range[2])
  })
    Miami_topoData <- st_read("/Users/allymuszynski/Desktop/GIS3FIN/topo/Miamitopo.shp")
    
    output$Miami_topoData <- renderLeaflet({
    min_max <- tm_shape(Miami_topo_NAD83) + tm_fill(data, title=input$Miami_topo_NAD83_sf, style="jenks") 
    tmap_leaflet(min_max)
    })
    # Generate summary of table
    output$sum <- renderPrint({
        
        MiamiData <- readOGR(".","Miami_shpv")
        Miami_topo <- readOGR(".","Miami_topo_NAD83")
        summary(MiamiData@data[,2:5])
    })
 }

# Run the app ----
shinyApp(ui = ui, server = server)