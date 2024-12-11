install.packages("maps")

# Load libraries
library(ggplot2)
library(maps)
library(tidyverse)
library(dplyr)
library(plotly)


clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

clim$altitude = as.numeric(gsub(",", "", clim$altitude))
clim$p_mean = as.numeric(gsub(",", "", clim$p_mean))

view(clim)

# Get map data for France
france_map <- map_data("france")

# Plot the map with points
ggplot() +
  # Plot the map
  geom_polygon(data = france_map, aes(x = long, y = lat, group = group), 
               fill = "grey", color = "blue") +
  # Add points
  geom_point(data = clim, aes(x = lon, y = lat), 
             color = "red", size = 6) +
  # Add labels
  geom_text(data = clim, aes(x = lon, y = lat, label = station), 
            vjust = -1, size = 4, color = "black") +
  # Map styling
  ggtitle("Map of France with Points") +
  theme_minimal()


# Question 2

climfrar <- clim[1:34,]

clim_model <- lm(t_mean ~ altitude + lat + lon, data = climfrar)

summary(clim_model)

# Interpretation:
#
# Intercept: 37.27
# When altitude, latitude and longitude are all 0, the model predicts an annual mean temperature of approximately 37.27 degrees Celsius.

# Altitude: -0.00641
# For every 1-metre increase in altitude, the annual mean temperature decreases by approximately 0.00641 degrees Celsius while holding latitude and longitude constant. This strong negative relationship is statistically significant (p < 0.001).

# Latitude: -0.53396
# For every 1-degree increase in latitude, the mean temperature decreases by approximately 0.534 degree Celsius while holding altitude and longitude constant. Meaning, moving further from the equator (higher latitude) corresponds to cooler temperatures. This relationship is highly significant (p < 0.001).

# Longitude: 0.03210
# For every 1-degree increase in longitude, the annual mean temperature increases by approximately 0.032 degree Celsius while holding altitude and latitude constant. However, the p-value (0.424) indicates this relationship is not statistically significant. Longitude does not appear to be a meaningful predictor in this model.

# Altitude and Latitude are significant predictors as they have a statistically significant value (p < 0.001) and Longitude is not statistically significant with a p-value of 0.424.

# Multiple R-squared: 0.8329 means 83.29% of the variability in annual mean temperature is explained by the model.


# Question 3

# Since Longitude was not statistically significant, I have chosen to exclude it from the model

clim_model_ex <- lm(t_mean ~ altitude + lat , data = climfrar)

# prediction_data %>% filter(clim, station == 'Mont-Ventoux' & station == 'Pic-du-midi')
# prediction_data <- clim[clim$station %in% c("Mont-Ventoux","Pic-du-midi"),]
prediction_data <- clim[35:36,]

# Predict on the subset
prediction_data$predicted_t_mean <- predict(clim_model_ex, newdata = prediction_data)

# View the subset with predictions
print(prediction_data)

summary(clim_model_ex)

# Intepretation

# Intercept: 37.91
# When both altitude and latitude are zero, the predicted annual mean temperature is 37.91.

# Altitude: -0.00626
# For every 1-metre increase in altitude, the annual mean temperature decreases by 0.00626 dgrees Celsius that is while holding latitude constant. The effect is highly significant  with a p-value = 2.34e-08.

#Latitude: -0.54653
# For every 1-degree increase in latitude, the annual mean temperature decreases by 0.54653 degrees Celsius while holding altitude constant. This effect is also highly significant as the p-value = 1.72e-11.

#R-squared: 0.8292 (82.92%)
# The model explains 82.92% of the variability in annual mean temperature, indicating a strong fit of the model to the data.


# Question 3

# Create a 3D scatterplot with the predictors and response variable
fig <- plot_ly(
  data = prediction_data,
  x = ~altitude,
  y = ~lat,
  z = ~t_mean,
  color = ~predicted_t_mean, # Color by predicted value
  colors = colorRamp(c("blue", "red")), # Gradient color
  marker = list(size = 5)
) %>%
  add_markers() %>%
  layout(
    title = "3D Scatterplot of Predicted Annual Mean Temperature For Mont-Ventoux and Pic-du-Midi",
    scene = list(
      xaxis = list(title = "Altitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Mean Temperature")
    )
  )

# Display the plot
fig





























