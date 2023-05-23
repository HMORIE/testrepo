# load required libraries
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
  
  # Render output plots with an id defined in ui.R
  output$city_bike_map <- renderLeaflet({
    leaflet(city_weather_bike_df) %>%
      addCircleMarkers(
        lng = ~LNG,
        lat = ~LAT,
        color = ~color_levels(BIKE_PREDICTION_LEVEL),
        radius = ~ifelse(BIKE_PREDICTION_LEVEL == "small", 6,
                         ifelse(BIKE_PREDICTION_LEVEL == "medium", 10, 12)),
        popup = ~LABEL
      )
  })
  
  # Render a leaflet map with circle markers and popup weather LABEL for all cities
  output$city_bike_map_all <- renderLeaflet({
    if (input$city_select == "All") {
      leaflet(city_weather_bike_df) %>%
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
  
  # Render temperature trend plot for all cities or a specific city
  output$temperature_trend_plot <- renderPlot({
    if (input$city_select == "All") {
      # Plot temperature trend for all cities
      ggplot(city_weather_bike_df, aes(x = DATE, y = TEMPERATURE)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = TEMPERATURE), nudge_y = 1) +
        labs(x = "Date", y = "Temperature") +
        theme_minimal()
    } else {
      # Plot temperature trend for a specific city
      selected_city <- input$city_select
      city <- city_weather_bike_df %>%
        filter(CITY_NAME == selected_city)
      
      ggplot(city, aes(x = DATE, y = TEMPERATURE)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = TEMPERATURE), nudge_y = 1) +
        labs(x = "Date", y = "Temperature") +
        theme_minimal()
    }
  })
  
  # Render bike-sharing demand prediction trend plot for all cities or a specific city
  output$bike_line <- renderPlot({
    if (input$city_select == "All") {
      # Plot bike-sharing demand prediction trend for all cities
      ggplot(city_weather_bike_df, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = BIKE_PREDICTION), nudge_y = 1) +
        labs(x = "Forecast Datetime", y = "Bike-Sharing Demand Prediction") +
        theme_minimal()
    } else {
      # Plot bike-sharing demand prediction trend for a specific city
      selected_city <- input$city_select
      city <- city_weather_bike_df %>%
        filter(CITY_NAME == selected_city)
      
      ggplot(city, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = BIKE_PREDICTION), nudge_y = 1) +
        labs(x = "Forecast Datetime", y = "Bike-Sharing Demand Prediction") +
        theme_minimal()
    }
  })
  
  # Text output for clicked x and y values in bike-sharing demand prediction trend plot
  output$bike_date_output <- renderText({
    clicked_x <- nearPoints(city_weather_bike_df, input$plot_click, xvar = "FORECASTDATETIME")$FORECASTDATETIME
    clicked_y <- nearPoints(city_weather_bike_df, input$plot_click, yvar = "BIKE_PREDICTION")$BIKE_PREDICTION
    paste("Clicked Datetime: ", clicked_x, "\nClicked Bike Prediction: ", clicked_y)
  })
  
  # Render a leaflet map with one marker on the map and a popup with DETAILED_LABEL displayed for a specific city
  output$city_bike_map_selected <- renderLeaflet({
    if (input$city_select != "All") {
      selected_city <- input$city_select
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
