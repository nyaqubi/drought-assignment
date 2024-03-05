#install.packages("raster")
library(raster)
#install.packages("sf")
require(sf)
#library(abind)
#library(fields)
#install.packages("tidyverse")
#library(maps)
#library(ncdf4)
library(ncdf4)
library(tidyr)
require(tidyverse)
require(ggplot2)
# Load library
#library(ncdf4)
# Load timeseries library
library(xts)
# Load library to compute geographical distance
library(sp)
library(geosphere)
#install.packages("data.table")
#library(data.table)
library(zoo)
#install.packages("dplyr")
#install.packages("dplyr")
library(dplyr)
#install.packages("vctrs")
library(vctrs)
########################


process_temperature_nc_file <- function(file_path) {
  # Open netCDF file
  ncin <- nc_open(file_path)
  
  # Extract necessary variables
  longitude <- ncvar_get(ncin, "lon")
  latitude <- ncvar_get(ncin, "lat")
  time <- ncvar_get(ncin, "time")
  temperature <- ncvar_get(ncin, "tas")
  
  # Additional processing
  is_na_check <- any(is.na(temperature))
  
  # Remove NA values from temperature
  non_na_indices <- which(!is.na(temperature))
  temperature <- temperature[non_na_indices]
  time <- time[non_na_indices]
  
  actual_dates <- as.Date(time, origin = "1949-12-01")
  temperature_numeric <- as.numeric(temperature) - 273.15  # Convert Kelvin to Celsius
  
 
  
  # Specify the variable and coordinates
  variable_name <- "tas"
  city_longitude <- 62.199074
  city_latitude <- 34.343044
  
  # Create mesh points coordinates
  coords_matrix <- cbind(as.vector(longitude), as.vector(latitude))
  mesh_points <- SpatialPoints(coords_matrix, proj4string = CRS("+proj=longlat +datum=WGS84"), bbox = NULL)
  city_point <- SpatialPoints(matrix(c(city_longitude, city_latitude), nrow = 1, ncol = 2), proj4string = CRS("+proj=longlat +datum=WGS84"), bbox = NULL)
  
  # ID of the closest point to the city in the mesh
  closest_point_id <- which.min(distGeo(mesh_points, city_point))
  
  # Construct a matrix with the points ID
  point_id_matrix <- matrix(1:length(latitude), nrow = nrow(latitude), ncol = ncol(latitude))
  
  # Get index i, j of the closest point
  closest_point_index <- which(point_id_matrix == closest_point_id, arr.ind = TRUE)
  
  # Read the temperature data at the specified longitude and latitude
  temperature_timeseries <- ncvar_get(ncin, varid = variable_name, start = c(closest_point_index[1], closest_point_index[2], 1), count = c(1, 1, -1))
  
  # Ensure that lengths match
  min_length <- min(length(temperature_timeseries), length(actual_dates))
  temperature_timeseries <- temperature_timeseries[1:min_length]
  actual_dates <- actual_dates[1:min_length]
  
  temperature_xts <- xts(temperature_timeseries, actual_dates)
  
  # Close the netCDF file
  nc_close(ncin)
  
  # Return the temperature time series data without NA values
  return(temperature_xts)
}

netcdf_files <- c(
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19610101-19651231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19660101-19701231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19710101-19751231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19760101-19801231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19810101-19851231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19860101-19901231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-19951231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc",
  "Historical/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20060101-20101231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20110101-20151231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20160101-20201231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc",
  "RCP85/Temperature/tas_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc"
)
all_temperature_timeseries <- list()

# Process each NetCDF file using a for loop
for (file_path in netcdf_files) {
  temperature_timeseries <- process_temperature_nc_file(file_path)
  all_temperature_timeseries[[length(all_temperature_timeseries) + 1]] <- temperature_timeseries
}


# Combine all processed temperature time series into a single data frame
combined_temperature_timeseries <- do.call(cbind, all_temperature_timeseries)

combined_temperature_long <- gather(data.frame(Date = index(combined_temperature_timeseries), combined_temperature_timeseries), key = "File", value = "Temperature", -Date)

# Convert temperature from Kelvin to Celsius
combined_temperature_long$Temperature <- combined_temperature_long$Temperature - 273.15
combined_temperature_long
# Split from 2006
split_date <- as.Date("2006-01-01")

# Create ColorGroup column
combined_temperature_long$ColorGroup <- ifelse(combined_temperature_long$Date >= split_date, "RCP85 Projection", "Historical Data")
combined_temperature_long
# Calculate annual averages
annual_averages <- combined_temperature_long %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Year, ColorGroup) %>%
  summarize(AverageTemperature = mean(Temperature, na.rm = TRUE))
#######
#######
# Calculate annual averages, maximums, and minimums
temperature_summary <- combined_temperature_long %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Year, ColorGroup) %>%
  summarize(
    AverageTemperature = mean(Temperature, na.rm = TRUE),
    MaxTemperature = max(Temperature, na.rm = TRUE),
    MinTemperature = min(Temperature, na.rm = TRUE)
  )

# Print the summary
print(temperature_summary)


# Convert Year to numeric type
temperature_summary$Year <- as.numeric(as.character(temperature_summary$Year))

# Create a column to specify the color based on the year
temperature_summary$ColorCategory <- ifelse(temperature_summary$Year < 2006, "Historical Data", "RCP85 Projection")

# Create a box plot for mean, max, and min temperatures with facets

########
######

#######

ggplot(temperature_summary, aes(x = Year, y = AverageTemperature, fill = ColorCategory)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7) +
  geom_point(aes(y = MaxTemperature, color = "Max Temperature"), position = position_dodge(width = 0.8), size = 2, shape = 4) +
  geom_point(aes(y = MinTemperature, color = "Min Temperature"), position = position_dodge(width = 0.8), size = 2, shape = 4) +  
  labs(title = "",
       y = "Temperature (C)",
       x = "Year") +
  scale_fill_manual(values = c("blue", "red"), name = "Legend",
                    labels = c("Historical Data", "RCP85 Projection")) +
  scale_color_manual(values = c("Max Temperature" = "orange", "Min Temperature" = "purple", "Historical Data" = "blue", "RCP85 Projection" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "vertical",
    legend.text = element_text(size = 8),  # Adjust the size to your liking
    legend.title = element_blank(),  # Removes the heading title for the temperature type
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)  # Adjust the angle and justification
  ) +
  scale_x_continuous(breaks = seq(min(temperature_summary$Year), max(temperature_summary$Year), by = 15))

############ Seasonal Analysis 
# Convert the Date column to a date format if it's not already
combined_temperature_long$Date <- as.Date(combined_temperature_long$Date)

# Extract the month from the Date column
combined_temperature_long$Month <- format(combined_temperature_long$Date, "%m")

# Convert the Month column to a numeric format
combined_temperature_long$Month <- as.numeric(combined_temperature_long$Month)

# Now add a new column for the season
combined_temperature_long$Season <- case_when(
  combined_temperature_long$Month %in% c(12, 1, 2) ~ "Winter",
  combined_temperature_long$Month %in% c(3, 4, 5) ~ "Spring",
  combined_temperature_long$Month %in% c(6, 7, 8) ~ "Summer",
  combined_temperature_long$Month %in% c(9, 10, 11) ~ "Autumn",
  TRUE ~ NA_character_
)

# Calculate the seasonal averages
seasonal_temperature_stats <- combined_temperature_long %>%
  group_by(Year, Season, ColorGroup) %>%
  summarize(
    AverageTemperature = mean(Temperature, na.rm = TRUE),
    MaxTemperature = max(Temperature, na.rm = TRUE),
    MinTemperature = min(Temperature, na.rm = TRUE)
  )

# Print the seasonal temperature statistics
print("Seasonal Temperature Statistics:")
print(seasonal_temperature_stats)

#########
ggplot(seasonal_temperature_stats, aes(x = as.numeric(Year), y = AverageTemperature, group = ColorGroup, color = ColorGroup)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype=5, size=0.5, colour="black")+
  labs(title = "",
       y = "Temperature (C)",
       x = "Year")+
  labs(title = "Seasonal Average Temperature", y = "Average Temperature (C)") +
  theme_minimal() +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red")) +
  facet_wrap(~ Season, ncol = 1) +
  scale_x_continuous(breaks = seq(min(seasonal_temperature_stats$Year), max(seasonal_temperature_stats$Year), by = 20)) +
  theme(legend.position = c(0, 1.15), legend.justification = c(0, 1), legend.title = element_blank())

############ Box plots 
ggplot(seasonal_temperature_stats, aes(x = Season, y = AverageTemperature, fill = ColorGroup)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Seasonal Temperature Variation from 1961-2100", y = "Average Temperature") +
  scale_fill_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name=NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Legend"))
#########
# Convert the Date column to a date format if it's not already
combined_temperature_long$Date <- as.Date(combined_temperature_long$Date)

# Extract the month from the Date column
combined_temperature_long$Month <- format(combined_temperature_long$Date, "%B")

# Calculate the monthly averages
monthly_temperatures <- combined_temperature_long %>%
  group_by(Year, Month, ColorGroup) %>%
  summarise(
    AverageMonthlyTemperature = mean(Temperature, na.rm = TRUE),
    MaxTemperature = max(Temperature, na.rm = TRUE),
    MinTemperature = min(Temperature, na.rm = TRUE)
  )

# Assign names to the months
month.order <- c("December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November")

# Reorder the levels of the Month factor
monthly_temperatures$Month <- factor(monthly_temperatures$Month, levels = month.order)
# Create the plot for monthly temperature variation
ggplot(monthly_temperatures, aes(x = as.factor(Year), y = AverageMonthlyTemperature, group = ColorGroup, color = ColorGroup, Month)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = 5, size = 0.5, colour = "black") +
  labs(title = "Monthly Temperature Variation", y = "Average Temperature (°C)", x = "Year") +
  theme_minimal() +
  theme(
    legend.position = c(0, 1.135),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8)  # Adjust the size to your liking
  ) +
  scale_x_discrete(breaks = seq(min(monthly_temperatures$Year), max(monthly_temperatures$Year), by = 30)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name = NULL) +
  facet_wrap(~ Month, scales = "free_x", nrow = 4, ncol = 3)
######### monthly box plot

# Assuming "Month" is a character column, convert it to a factor with desired levels
combined_temperature_long$Month <- factor(combined_temperature_long$Month, levels = c("December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November"))

ggplot(combined_temperature_long, aes(x = Month, y = Temperature, fill = ColorGroup)) +
  geom_boxplot() +
  labs(title = "Temperature Variation Plot", y = "Temperature (°C)", x = "Month") +
  scale_fill_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),  # Adjust the size to your liking
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
