# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)

# Create a Shiny UI
shinyUI(
  fluidPage(padding = 5,
            titlePanel("Bike Sharing Demand Prediction App"),
            # Create a sidebar layout
            sidebarLayout(
              # Create a main panel to show cities on a leaflet map
              mainPanel(
                leafletOutput('city_bike_map', height = 600, width = 600)
              ),
              # Create a sidebar to show detailed plots for a city
              sidebarPanel(
                selectInput("city_select", "Select a city:", choices = c("All", "Seoul", "New York", "Paris", "London", "Suzhou")), # select drop-down list to select city and plot output
                plotOutput("temp_line"),  # Add plot output with ID "temp_line"
                plotOutput("bike_line", click = "plot_click"),  # Add plot output with ID "bike_line"
                verbatimTextOutput("bike_date_output")
              )
            )
  )
)
