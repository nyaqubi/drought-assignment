#install.packages("raster")
library(raster)
#install.packages("sf")
require(sf)
#library(abind)
#library(fields)
#install.packages("tidyverse")
#library(maps)
#install.packages("gridExtra")
library(gridExtra)
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
library(lubridate)
# Install and load the dplyr package
library(dplyr)
########################
process_nc_file <- function(ncfile) {
  # Open netCDF file
  ncin <- nc_open(ncfile)
  
  # Extract necessary variables
  lon <- ncvar_get(ncin, "lon")
  lat <- ncvar_get(ncin, "lat")
  t <- ncvar_get(ncin, "time")
  rain <- ncvar_get(ncin, "pr")
  
  # Additional processing
  nacheck <- any(is.na(rain))
  actualdates <- as.Date(t, origin = "1949-12-01")
  rain_n <- as.numeric(rain)
  rain_mmd <- rain_n * 86400
  
  # Specify the variable and coordinates
  variable_name <- "pr"
  lon_city <- 62.199074
  lat_city <- 34.343044
  
  # Create mesh points coordinates
  coords_matrix <- cbind(as.vector(lon), as.vector(lat))
  mesh_points <- SpatialPoints(coords_matrix, proj4string = CRS("+proj=longlat +datum=WGS84"), bbox = NULL)
  city_point <- SpatialPoints(matrix(c(lon_city, lat_city), nrow = 1, ncol = 2), proj4string = CRS("+proj=longlat +datum=WGS84"), bbox = NULL)
  
  # ID of the closest point to city in the mesh
  closest_point <- which.min(distGeo(mesh_points, city_point))
  
  # Contruct a matrix with the points ID
  point_ID_matrix <- matrix(1:length(lat), nrow = nrow(lat), ncol = ncol(lat))
  # Get index i, j of the closest point
  ij <- which(point_ID_matrix == closest_point, arr.ind = TRUE)
  
  # Read the variable data at the specified longitude and latitude
  timeseries <- ncvar_get(ncin, varid = variable_name, start = c(ij[1], ij[2], 1), count = c(1, 1, -1))
  timeseries <- timeseries * 86400
  timeseries_xts <- xts(timeseries, actualdates)
  # Return the timeseries data instead of plotting
  return(timeseries_xts)
  
  # Close the netCDF file
  nc_close(ncin)
}

############
ncfiles <- c(
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19610101-19651231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19660101-19701231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19710101-19751231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19760101-19801231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19810101-19851231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19860101-19901231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-19951231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc",
  "Historical/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20060101-20101231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20110101-20151231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20160101-20201231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc",
  "RCP85/Precipitation/pr_WAS-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc"
)

# Create an empty list to store processed time series
all_timeseries <- list()

# Process each NetCDF file using a for loop
for (ncfile in ncfiles) {
  timeseries <- process_nc_file(ncfile)
  all_timeseries[[length(all_timeseries) + 1]] <- timeseries
}

# Combine all processed time series into a single data frame
combined_timeseries <- do.call(cbind, all_timeseries)

combined_timeseries_long <- gather(data.frame(Date = index(combined_timeseries), combined_timeseries), key = "File", value = "Precipitation", -Date)

# Set the specific date to split the time series
split_date <- as.Date("2006-01-01")

# Create a new column to indicate the color group
combined_timeseries_long$ColorGroup <- ifelse(combined_timeseries_long$Date >= split_date, "RCP85 Projection", "Historical Data")


# Plot using ggplot2
ggplot(combined_timeseries_long, aes(x = Date, y = Precipitation, group = File, color = ColorGroup)) +
  geom_line() +
  labs(title = "Daily Precipitation Time Series", y = "Precipitation (mm/day)") +
  theme_minimal() +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name=NULL)
#################
# Plot using ggplot2
# Combine all processed time series into a single data frame
# Extract year from the Date column
combined_timeseries_long <- combined_timeseries_long %>%
  mutate(Year = as.numeric(format(Date, "%Y")))

# Convert 'Precipitation' to numeric (in case it is not already numeric)
combined_timeseries_long$Precipitation <- as.numeric(combined_timeseries_long$Precipitation)

combined_timeseries_long
# Calculate yearly mean, max, and min

################################
# Convert the Date column to a date format if it's not already
combined_timeseries_long$Date <- as.Date(combined_timeseries_long$Date)

# Extract the month from the Date column
combined_timeseries_long$Month <- format(combined_timeseries_long$Date, "%m")

# Convert the Month column to a numeric format
combined_timeseries_long$Month <- as.numeric(combined_timeseries_long$Month)

# Now you can add a new column for the season
combined_timeseries_long$Season <- case_when(
  combined_timeseries_long$Month %in% c(12, 1, 2) ~ "Winter",
  combined_timeseries_long$Month %in% c(3, 4, 5) ~ "Spring",
  combined_timeseries_long$Month %in% c(6, 7, 8) ~ "Summer",
  combined_timeseries_long$Month %in% c(9, 10, 11) ~ "Autumn",
  TRUE ~ NA_character_
)

# Calculate the seasonal averages
seasonal_stats <- combined_timeseries_long %>%
  group_by(Year, Season, ColorGroup) %>%
  summarise(
    Total_seasonal_Precipitation = sum(Precipitation, na.rm = TRUE),
    mean_seasonal_Precipitation = mean(Precipitation, na.rm = TRUE),
    Max_Precipitation = max(Precipitation, na.rm = TRUE),
    Min_Precipitation = min(Precipitation, na.rm = TRUE)
  )
max_max_precipitation <- max(seasonal_stats$Max_Precipitation, na.rm = TRUE)
print(max_max_precipitation)
# Print the results
print("Seasonal Statistics:")

##########
#### Seasonal Average Values
ggplot(seasonal_stats, aes(x = Year, y = Total_seasonal_Precipitation, group = ColorGroup, color = ColorGroup)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype=5, size=0.5, colour="black") +
  labs(title = "Maximum Seasonal Precipitation", y = "Total Precipitation (mm/season)") +
  theme_minimal() +
  theme(legend.position = c(0, 1.1), legend.justification = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(seasonal_stats$Year), max(seasonal_stats$Year), by = 10)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red","Trend Line"="black"), name=NULL) +
  facet_wrap(~ Season, ncol = 1)
#######
ggplot(seasonal_stats, aes(x = Season, y = Total_seasonal_Precipitation, fill = ColorGroup)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Seasonal Precipitation", y = "Total Precipitation (mm/season)") +
  scale_fill_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name=NULL) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Description"))

#### Seasonal Max Values
p_max <- ggplot(seasonal_stats, aes(x = Year, y = Max_Precipitation, group = ColorGroup, color = ColorGroup)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype=5, size=0.5, colour="black") +
  labs(title = "Maximum Daily Precipitation of Season", y = "Total Precipitation (mm/season)") +
  theme_minimal() +
  theme(legend.position = c(0, 1.1), legend.justification = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(seasonal_stats$Year), max(seasonal_stats$Year), by = 10)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red","Trend Line"="black"), name=NULL) +
  facet_wrap(~ Season, ncol = 1) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 100))
p_min <- ggplot(seasonal_stats, aes(x = Year, y = Min_Precipitation, group = ColorGroup, color = ColorGroup)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype=5, size=0.5, colour="black") +
  labs(title = "Minimum Daily Precipitation of Season", y = "Total Precipitation (mm/season)") +
  theme_minimal() +
  theme(legend.position = c(0, 1.1), legend.justification = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(seasonal_stats$Year), max(seasonal_stats$Year), by = 10)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red","Trend Line"="black"), name=NULL) +
  facet_wrap(~ Season, ncol = 1) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 100))

grid.arrange(p_max, nrow=1)
##### Monthly Average values

# Convert 'Precipitation' to numeric (in case it is not already numeric)
combined_timeseries_long$Precipitation <- as.numeric(combined_timeseries_long$Precipitation)

# Convert the Date column to a date format if it's not already
combined_timeseries_long$Date <- as.Date(combined_timeseries_long$Date)

# Extract the month from the Date column
combined_timeseries_long$Month <- format(combined_timeseries_long$Date, "%B")

# Calculate the monthly averages
monthly_totals <- combined_timeseries_long %>%
  group_by(Year, Month, ColorGroup) %>%
  summarise(
    Total_monthly_Precipitation = sum(Precipitation, na.rm = TRUE),
    mean_monthly_Precipitation = mean(Precipitation, na.rm = TRUE),
    Max_Precipitation = max(Precipitation, na.rm = TRUE),
    Min_Precipitation = min(Precipitation, na.rm = TRUE)
  )

print(monthly_totals)
# Assign names to the months
month.order <- c("December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November")

# Reorder the levels of the Month factor
monthly_totals$Month <- factor(monthly_totals$Month, levels = month.order)
#####
# Create the plot
ggplot(monthly_totals, aes(x = Year, y = Total_monthly_Precipitation, group = ColorGroup, color = ColorGroup, Month_Name)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE,linetype=5, size=0.5, colour="black") +
  labs(title = "Total Monthly Mean Precipitation", y = "Mean Precipitation (mm/month)") +
  theme_minimal() +
  theme(
    legend.position = c(0, 1.135),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8)  # Adjust the size to your liking
  )+
  scale_x_continuous(breaks = seq(min(monthly_totals$Year), max(monthly_totals$Year), by = 30)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name=NULL) +
  facet_wrap(~ Month, nrow = 4, ncol = 3)

# Create the plot
ggplot(monthly_totals, aes(x = Month, y = Total_monthly_Precipitation, fill = ColorGroup)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Monthly Precipitation", y = "Total Precipitation (mm/month)") +
  scale_fill_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name=NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.3, 0.97),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8) ,  # Adjust the size to your liking
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)  # Adjust the angle and justification
  ) +
  guides(fill = guide_legend(title = "Description"))

# Print the results
print(monthly_totals)
##########



##########################
yearly_total <- combined_timeseries_long %>%
  group_by(Year, ColorGroup) %>%
  summarise(
    Total_Precipitation = sum(Precipitation, na.rm = TRUE),
    Max_Precipitation = max(Precipitation, na.rm = TRUE),
    Min_Precipitation = min(Precipitation, na.rm = TRUE)
  )

# Print the results
print("Yearly Statistics:")
print(yearly_total)

ggplot(yearly_total, aes(x = Year, y = Total_Precipitation, group = ColorGroup, color = ColorGroup)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype=3, colour='black', size=0.8) +
  labs(title = "Annual Average Precipitation", y = "Mean Precipitation (mm/yearly)") +
  theme_minimal() +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(yearly_total$Year), max(yearly_total$Year), by = 10)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name=NULL)
#######

# Calculate the yearly statistics for every ten years


# Create the box plot
ggplot(yearly_total, aes(x = Year, y = Total_Precipitation, fill = ColorGroup)) +
  geom_boxplot() +
  labs(title = "Annual Precipitation", y = "Precipitation (mm/year)") +
  scale_fill_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red"), name=NULL) +
  theme_minimal()+
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8) ,  # Adjust the size to your liking
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)  # Adjust the angle and justification
  ) +
  guides(fill = guide_legend(title = "Description"))

######

#######
####
###
getwd()
cwd <- getwd()
# List the contents of the working directory
directory_contents <- list.files(cwd)
directory_contents
historical_1 <- read_csv(file = 'NHS-79-2023.csv')
########
# Read the CSV file
historical_1 <- read.csv(file.path(cwd, 'NHS-79-2023.csv'), header = TRUE)

# Assuming there's a 'Year' column, order the data by 'Year'
historical_1 <- historical_1[order(historical_1$Year), ]

# Print or use the data as needed

###########
# Assuming your dataframe is historical_1 and columns are named from 'January' to 'December'
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Use the melt function to reshape the data
Measured <- melt(historical_1, id.vars = "Year", measure.vars = months, variable.name = "Month", value.name = "Data")
# Now, historical_long contains the reshaped data
str(Measured)

# Sort the data by 'Year'
Measured <- arrange(historical_long_1, Year)

# Now, historical_long is sorted by 'Year' from 1979 to 2007
# Load the necessary libraries


# Calculate the monthly averages for combined_timeseries_long
monthly_totals <- combined_timeseries_long %>%
  group_by(Year, Month, ColorGroup) %>%
  summarise(
    Total_monthly_Precipitation = sum(Precipitation, na.rm = TRUE),
    mean_monthly_Precipitation = mean(Precipitation, na.rm = TRUE),
    Max_Precipitation = max(Precipitation, na.rm = TRUE),
    Min_Precipitation = min(Precipitation, na.rm = TRUE)
  )
monthly_totals
# Calculate the monthly averages for Measured
# Calculate the monthly averages for Measured
M_monthly_totals <- Measured %>%
  group_by(Year, Month) %>%
  summarise(
    Total_monthly_Precipitation = sum(Data, na.rm = TRUE),
    mean_monthly_Precipitation = mean(Data, na.rm = TRUE),
    Max_Precipitation = ifelse(all(is.na(Data)), NA, max(Data, na.rm = TRUE)),
    Min_Precipitation = ifelse(all(is.na(Data)), NA, min(Data, na.rm = TRUE))
  )
M_monthly_totals

# Add ColorGroup for Measured Data
M_monthly_totals$ColorGroup <- "Measured Data"

M_monthly_totals
# Combine the two data frames
combined_data <- bind_rows(monthly_totals, M_monthly_totals)
combined_data
# Assign names to the months
month.order <- c("December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November")

# Reorder the levels of the Month factor
combined_data$Month <- factor(combined_data$Month, levels = month.order)
# Create the box plot
ggplot(combined_data, aes(x = Month, y = Total_monthly_Precipitation, fill = ColorGroup)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 0, fill = "white") +
  labs(title = "Monthly Precipitation Plot", y = "Total Precipitation (mm/month)") +
  scale_fill_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red", "Measured Data" = "orange"), name = NULL) +
  theme_minimal() +
  theme(
    legend.position = c(0.3, 0.97),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.text = element_text(size = 8),  # Adjust the size to your liking
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)  # Adjust the angle and justification
  ) +
  guides(fill = guide_legend(title = "Description"))

ggplot(combined_data, aes(x = Year, y = Total_monthly_Precipitation, group = ColorGroup, color = ColorGroup, Month)) +
  geom_line() +
  labs(title = "Total Monthly Mean Precipitation", y = "Mean Precipitation (mm/month)") +
  theme_minimal() +
  theme(
    legend.position = c(0, 1.135),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8)  # Adjust the size to your liking
  )+
  scale_x_continuous(breaks = seq(min(monthly_totals$Year), max(monthly_totals$Year), by = 30)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red", "Measured Data" = "orange"), name=NULL) +
  facet_wrap(~ Month, nrow = 4, ncol = 3)

# Filter the data for the months of interest
months_of_interest <- c("December", "January", "February", "March", "April", "November")
filtered_data <- combined_data %>% filter(Month %in% months_of_interest)

# Plot the data
ggplot(filtered_data, aes(x = Year, y = Total_monthly_Precipitation, group = ColorGroup, color = ColorGroup, Month)) +
  geom_line() +
  labs(title = "Total Monthly Mean Precipitation", y = "Mean Precipitation (mm/month)") +
  theme_minimal() +
  theme(
    legend.position = c(0, 1.135),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8)  # Adjust the size to your liking
  )+
  scale_x_continuous(breaks = seq(min(filtered_data$Year), max(filtered_data$Year), by = 30)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red", "Measured Data" = "orange"), name=NULL) +
  facet_wrap(~ Month, nrow = 2, ncol = 3)

# Filter the data for the months of interest
months_of_interest <- c("December", "February", "March")
filtered_data <- combined_data %>% filter(Month %in% months_of_interest)

# Plot the data
ggplot(filtered_data, aes(x = Year, y = Total_monthly_Precipitation, group = ColorGroup, color = ColorGroup, Month)) +
  geom_line() +
  labs(title = "Total Monthly Mean Precipitation", y = "Mean Precipitation (mm/month)") +
  theme_minimal() +
  theme(
    legend.position = c(0, 1.135),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8)  # Adjust the size to your liking
  )+
  scale_x_continuous(breaks = seq(min(filtered_data$Year), max(filtered_data$Year), by = 30)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red", "Measured Data" = "orange"), name=NULL) +
  facet_wrap(~ Month, nrow = 1, ncol = 3)

# Filter the data for the months of interest
months_of_interest <- c("January")
filtered_data <- combined_data %>% filter(Month %in% months_of_interest)

# Plot the data
ggplot(filtered_data, aes(x = Year, y = Total_monthly_Precipitation, group = ColorGroup, color = ColorGroup, Month)) +
  geom_line() +
  labs(title = "January Mean Precipitation", y = "Mean Precipitation (mm/month)") +
  theme_minimal() +
  theme(
    legend.position = c(0, 1.135),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8)  # Adjust the size to your liking
  )+
  scale_x_continuous(breaks = seq(min(filtered_data$Year), max(filtered_data$Year), by = 30)) +
  scale_color_manual(values = c("Historical Data" = "blue", "RCP85 Projection" = "red", "Measured Data" = "orange"), name=NULL) +
  facet_wrap(~ Month, nrow = 1, ncol = 1)
