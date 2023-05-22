# Install and import required libraries
library(shiny)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(httr)
library(scales)

# Import model_prediction.R which contains methods to call OpenWeather API
# and make predictions

source("model_prediction.R")

test_weather_data_generation <- function() {
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df) > 0)
  print(city_weather_bike_df)
  return(city_weather_bike_df)
}

# Create a function to generate city weather bike data
city_weather_bike_df <- test_weather_data_generation()

# Create a Shiny server
shinyServer(function(input, output) {
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"),
                              levels = c("small", "medium", "large"))
  
  # Create another data frame called `cities_max_bike` with each row containing city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII, LNG, LAT ) %>%
    summarize(max(BIKE_PREDICTION))
  
  # Render output plots with an id defined in ui.R
  output$city_bike_map <- renderLeaflet({
    leaflet(cities_max_bike) %>%
      addCircleMarkers(
        lng = ~LNG,
        lat = ~LAT,
        color = ~color_levels(BIKE_PREDICTION_LEVEL),
        radius = ~ifelse(BIKE_PREDICTION_LEVEL == "small", 6,
                         ifelse(BIKE_PREDICTION_LEVEL == "medium", 10, 12)),
        popup = ~LABEL
      )
  })
  
  # Render a leaflet map with circle markers and popup weather LABEL for all five cities
  output$city_bike_map_all <- renderLeaflet({
    if (input$city_selection == "All") {
      leaflet(cities_bike_pred) %>%
        addCircleMarkers(
          lng = ~LNG,
          lat = ~LAT,
          color = ~color_levels(BIKE_PREDICTION_LEVEL),
          radius = ~ifelse(BIKE_PREDICTION_LEVEL == "small", 6,
                           ifelse(BIKE_PREDICTION_LEVEL == "medium", 10, 12))
        ) %>%
        addPopups(
          lng = ~LNG,
          lat = ~LAT,
          popup = ~DETAILED_LABEL
        )
    }
  })
  
  # Render a leaflet map with one marker on the map and a popup with DETAILED_LABEL displayed for a specific city
  output$city_bike_map_selected <- renderLeaflet({
    if (input$city_selection != "All") {
      selected_city <- input$city_selection
      city <- city_weather_bike_df %>%
        filter(CITY_NAME == selected_city)
      
      leaflet(city) %>%
        addMarkers(
          lat = ~LAT,
          lng = ~LNG,
          label = ~DETAILED_LABEL
        )
    }
  })
  
})
