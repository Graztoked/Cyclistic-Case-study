library(tidyverse)
library(purrr)
library(tidyr)
library(janitor)
library(here)
library(skimr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

#loading library that we are using.

#---------------data importing--------------

#Imports the past 12 months of data
setwd("Divvy TripData Past 12 Months")
# Set the working directory to "Divvy Trip Data Past 12 Months"

table_names <- list.files(full.names = TRUE)
# Creates a vector that has all the file names in the current working directory

full_dirty_data <- data.frame()
for (i in 1:length(table_names)) {
  full_dirty_data <- rbind(full_dirty_data, read.csv(table_names[i]))
}
# Create an empty data frame and then rbinds all the CSV files in the current working directory into one data frame

setwd("..")
# Bring you back to the parent directory.

rm(i, table_names)
#cleans up variables, just leaves full_dirty_data.

#-------------------quick data clean---------------------------

# Clean the column names
full_dirty_data <- clean_names(full_dirty_data)

# Round the columns to the nearest 0.01
full_dirty_data$start_lat <- round(full_dirty_data$start_lat, 2)
full_dirty_data$start_lng <- round(full_dirty_data$start_lng, 2)
full_dirty_data$end_lat <- round(full_dirty_data$end_lat, 2)
full_dirty_data$end_lng <- round(full_dirty_data$end_lng, 2)

unique_data <- distinct(full_dirty_data)
# Creates a data.frame with no duplicate data within it.

duplicate_data <- full_dirty_data[duplicated(full_dirty_data) |
                                    duplicated(full_dirty_data, fromLast = TRUE),]
# Gives a data.frame with just duplicated data with original data for error checking

#-----Cleanup-------

rm(full_dirty_data, duplicate_data)

#glimpse(unique_data)
#Quick Summery

#------------------code chunk to check for member/casual counts--------

# Analysis checking for unique member_casual variables including null and blank values
member_casual_SP <- unique(unique_data$member_casual)

member_casual_count <- data.frame(
  unique_value = character(),
  count = integer(),
  na_count = integer(),
  blank_count = integer(),
  stringsAsFactors = FALSE
)

for (value in member_casual_SP) {
  count <- unique_data %>%
    filter(member_casual == value) %>%
    count(member_casual) %>%
    pull(n)
  
  na_count <- sum(is.na(unique_data$member_casual))
  
  blank_count <- ifelse(value == "", sum(unique_data$member_casual == ""), 0)
  
  member_casual_count <-
    rbind(member_casual_count,
          data.frame(unique_value = value, count = count, na_count = na_count, blank_count = blank_count))
}

member_casual_count <- member_casual_count[order(member_casual_count$unique_value),]

#code cleanup
rm(value)

#---cleanup---

rm(member_casual_count,blank_count,count,member_casual_SP,na_count)
# Cleans up the values used in this code chunk

#------------checking for for types of bikes used------------------

#Create a backup of data used
unique_bikes <- unique_data

# Analysis of how many types of bikes in ridable_type column
unique_bike_type <- unique(unique_bikes$rideable_type)

# Count null/blank values in rideable_type column
null_count <- sum(is.null(unique_bikes$rideable_type) | unique_bikes$rideable_type == "")

# Create a new data frame to count bike types used by casuals and members
bike_counts <- unique_bikes %>%
  group_by(member_casual, rideable_type) %>%
  summarise(count = n()) %>%
  ungroup()

# Check for zero counts and assign zero if necessary
bike_counts <- complete(
  bike_counts,
  member_casual,
  rideable_type,
  fill = list(count = 0)
)

# Calculate percentage
bike_counts <- bike_counts %>%
  group_by(member_casual) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

#------Plotting data------

#----Plot the relationship for casual vs members usage of types of bikes recorded---

ggplot(bike_counts, aes(x = rideable_type, y = count, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Bike Type", y = "Count", fill = "Member/Casual") +
  scale_fill_manual(values = c("blue", "orange"), labels = c("Casual", "Member")) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(bike_counts$count), 250000), 
                     labels = label_number(scale = 1e-3, suffix = "k"))
#----Plot the relationship for casual vs members usage of types of bikes recorded as Percentages----

ggplot(bike_counts, aes(x = rideable_type, y = percentage, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Bike Type", y = "Percentage", fill = "Member/Casual") +
  scale_fill_manual(values = c("blue", "orange"), labels = c("Casual", "Member")) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5))

#---cleanup---

#Cleans up Variables used with this analysis.
rm(unique_bikes,null_count,total_counts_ridable_type,bike_counts,unique_bike_type)


#--------------------Duplicate ride_id checking/Analysis------------------------
#Analysis checking for duplicate ride_id column

# Creates a data frame named ride_id_unique_count that gives the count of unique ride_id values.
ride_id_unique_count <- unique_data %>%
  distinct(ride_id) %>%
  count()

# Creates a data frame named ride_id_count that gives the count of total occurrences of each ride_id.
ride_id_count <- unique_data %>%
  count(ride_id)

# Creates a variable that calculates how many times ride_id value has been repeated for error checking.
repeated_ride_id_count <- sum(ride_id_count$n > 1)

# Creates a data.frame with what rider_id's that have been repeated
repeated_ride_ids <- unique_data %>%
  group_by(ride_id) %>%
  filter(n() > 1)

#---Cleanup---

rm(repeated_ride_ids,repeated_ride_id_count,ride_id_unique_count,ride_id_count)
# clean up the analysis of ride_id column

#-------------------------------Days of the week calculation------------------------------------------
# Temperary value to use in these calculations
day_of_week <- unique_data %>% 
  select(started_at, ended_at, member_casual)

#checking for n/a or blank values in the columns started_at, ended_at
na_blank_counts <- data.frame(
  column = c("started_at", "ended_at"),
  na_count = rep(0, 2),
  blank_count = rep(0, 2)
)

for (col in c("started_at", "ended_at")) {
  na_blank_counts$na_count[na_blank_counts$column == col] <- sum(is.na(day_of_week[[col]]))
  na_blank_counts$blank_count[na_blank_counts$column == col] <- sum(day_of_week[[col]] == "")
}
# Cleans up some n/a or blank value code
rm(col)

# Calculate time difference in minutes and rounds to the whole number
day_of_week$trip_time_min <- floor(as.numeric(difftime(day_of_week$ended_at, day_of_week$started_at, units = "mins")))

#---days of the week data conversion---

  # Create a new data frame for day of the week conversion
  converted_day_of_week <- data.frame(day_of_week)
  
  # Convert "started_at" and "ended_at" columns to Date format
  converted_day_of_week$started_at <- as.Date(converted_day_of_week$started_at)
  converted_day_of_week$ended_at <- as.Date(converted_day_of_week$ended_at)
  
  # Add new columns for day of the week in the converted data frame
  converted_day_of_week$started_day <- weekdays(converted_day_of_week$started_at)
  converted_day_of_week$ended_day <- weekdays(converted_day_of_week$ended_at)
  
  # Move the new columns to the original data frame
  day_of_week$started_day <- converted_day_of_week$started_day
  day_of_week$ended_day <- converted_day_of_week$ended_day

#-----end of the days of the week data conversion-----

# Check if "started_day" and "ended_day" are the same in converted_day_of_week
converted_day_of_week$is_same_day <- converted_day_of_week$started_day == converted_day_of_week$ended_day

# Count occurrences of same and different days in converted_day_of_week
converted_day_of_week$same_day_count <- sum(converted_day_of_week$is_same_day)
converted_day_of_week$different_day_count <- sum(!converted_day_of_week$is_same_day)

# Move the new columns to the original data frame
day_of_week$is_same_day <- converted_day_of_week$is_same_day
day_of_week$same_day_count <- converted_day_of_week$same_day_count
day_of_week$different_day_count <- converted_day_of_week$different_day_count

# Code cleanup
rm(converted_day_of_week)

#---------------------Plot Information------------------------

#----plot the relationship between days of the week for members vs casual----
  # Create a Filter to only include data for when the rows is_same_day is TRUE
  filtered_data <- day_of_week[day_of_week$is_same_day == TRUE, ]
  
  # Specify the levels of the started_day variable
  filtered_data$started_day <- factor(filtered_data$started_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # Plot the relationship between started_day and member_casual
  ggplot(filtered_data, aes(x = started_day, fill = member_casual)) +
    geom_bar(position = "fill") +
    labs(x = "Start Day", y = "Proportion", fill = "Member/Casual") +
    ggtitle("Relationship between Days of the Week for Members vs Casuals") +
    theme_minimal()

rm(filtered_data)

#----plot the total time spend for members and casuals----

# Calculate the total time spent for members and casuals and convert to hours
total_time <- day_of_week %>%
  group_by(member_casual) %>%
  summarize(total_time = sum(trip_time_min) / 60)  # Convert minutes to hours

# Plot the total time spent for members and casuals
ggplot(total_time, aes(x = member_casual, y = total_time, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(x = "Member/Casual", y = "Total Time Spent (hours)", fill = "Member/Casual") +
  ggtitle("Total Time Spent for Members vs Casuals in Hours") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

rm(total_time)

#----Plot the average time spent for members and casuals with a 5-minute interval scale----

# Calculate the average time spent for members and casuals
average_time <- day_of_week %>%
  group_by(member_casual) %>%
  summarize(average_time = mean(trip_time_min))

ggplot(average_time, aes(x = member_casual, y = average_time, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(x = "Member/Casual", y = "Average Time Spent", fill = "Member/Casual") +
  ggtitle("Average Time Spent for Members vs Casuals") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, ceiling(max(average_time$average_time)), 5))

rm(average_time)

#----plot the relationship between members vs casuals average trip time for day of the week----

# Filter the data for rows where is_same_day is TRUE
filtered_data <- day_of_week[day_of_week$is_same_day == TRUE, ]

# Specify the levels of the started_day variable
filtered_data$started_day <- factor(filtered_data$started_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Calculate the average time spent for members and casuals
average_time <- filtered_data %>%
  group_by(member_casual, started_day) %>%
  summarise(average_time = mean(trip_time_min))

# Plot the average time spent for members and casuals
ggplot(average_time, aes(x = started_day, y = average_time, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day of the Week", y = "Average Time Spent (in 5-minute intervals)", fill = "Member/Casual") +
  ggtitle("Average Time Spent for Members vs Casuals") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, ceiling(max(average_time$average_time)), 5))

rm(average_time,filtered_data)

#----plot the relationship between members vs casuals for trips going past 12am----

# Filter the data for trips that go past 12 PM
filtered_data <- day_of_week[day_of_week$is_same_day == FALSE, ]

# Plot the relationship between member_casual and trip_time_min
ggplot(filtered_data, aes(x = member_casual, fill = member_casual)) +
  geom_bar() +
  labs(x = "Member/Casual", y = "Count", fill = "Member/Casual") +
  ggtitle("Relationship between Members and Casuals for Trips past 12 PM") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 25000), breaks = seq(0, 30000, 2500))

rm(filtered_data)

#---cleanup---

rm(day_of_week, na_blank_counts)

#----------------------------cleaning up and analysisng stations------ do not use----------------------------------------
#-----------------------------------Station cleanup--------------------------------------------------
#---------------------create a lookup table for use with cleaning and analysis----------

# Creates a temperary data.frame to use
station_fixing <- unique_data

# Create a lookup table1
lookup_table1 <- data.frame()

# Populate lookup table with start_station_name, start_lat, start_lng, and start_station_id
lookup_table1 <- lookup_table1 %>%
  bind_rows(
    station_fixing %>%
      filter(!(is.na(start_station_name) | start_station_name == "")) %>%
      distinct(start_station_name, start_lat, start_lng, start_station_id)
  )

lookup_table1 <- lookup_table1 %>%
  rename(station_name = start_station_name, lat = start_lat, lng = start_lng, station_id = start_station_id)

# Create a lookup table2
lookup_table2 <- data.frame()

# Populate lookup table with end_station_name, end_lat, end_lng, and end_station_id
lookup_table2 <- lookup_table2 %>%
  bind_rows(
    station_fixing %>%
      filter(!(is.na(end_station_name) | end_station_name == "")) %>%
      distinct(end_station_name, end_lat, end_lng, end_station_id)
  )

lookup_table2 <- lookup_table2 %>%
  rename(station_name = end_station_name, lat = end_lat, lng = end_lng, station_id = end_station_id)

# Combine rows from lookup_table1 and lookup_table2
combined_table <- bind_rows(lookup_table1, lookup_table2)

# Create lookup_table with unique values
lookup_table <- distinct(combined_table)

# Remove unused tables.
rm(lookup_table1, lookup_table2, combined_table)

#---cleanup---

rm(lookup_table)

#------------code chunk to check how many stations will get fixed--------------
#--------initial count--------

# Count missing values in start_station_name
missing_start_counts <- station_fixing %>%
  filter(is.na(start_station_name) | start_station_name == "") %>%
  summarise(count = n(), station_type = "start_station_name")

# Count missing values in end_station_name
missing_end_counts <- station_fixing %>%
  filter(is.na(end_station_name) | end_station_name == "") %>%
  summarise(count = n(), station_type = "end_station_name")

# Count missing values in start_station_id
missing_start_id_counts <- station_fixing %>%
  filter(is.na(start_station_id) | start_station_id == "") %>%
  summarise(count = n(), station_type = "start_station_id")

# Count missing values in end_station_id
missing_end_id_counts <- station_fixing %>%
  filter(is.na(end_station_id) | end_station_id == "") %>%
  summarise(count = n(), station_type = "end_station_id")

# Combine all the counts into a single data frame
missing_station_name_starting_counts <- bind_rows(missing_start_counts, missing_end_counts, missing_start_id_counts, missing_end_id_counts)

# Remove the individual count data frames
rm(missing_start_counts, missing_end_counts, missing_start_id_counts, missing_end_id_counts)

#---cleanup---

rm(missing_station_name_starting_counts)

#-----------------------------fixing station names---------do not use-------------------------------------

# Filter rows with missing or N/A values in start_station_name and start_station_id
missing_start_rows <- station_fixing %>%
  filter(
    is.na(start_station_name) | start_station_name == "" |
      is.na(start_station_id) | start_station_id == ""
  )

# Filter rows with missing or N/A values in end_station_name and end_station_id
missing_end_rows <- station_fixing %>%
  filter(
    is.na(end_station_name) | end_station_name == "" |
      is.na(end_station_id) | end_station_id == ""
  )

# Create a data.frame that has the correct column names for the starting section of stations
lookup_table_start <- lookup_table %>%
  rename(start_station_name = station_name, start_lat = lat, start_lng = lng, start_station_id = station_id)

# Create a data.frame that has the correct column names for the ending section of stations
lookup_table_end <- lookup_table %>%
  rename(end_station_name = station_name, end_lat = lat, end_lng = lng, end_station_id = station_id)

# Loop through missing_start_rows
missing_start_rows$start_station_name <- NA
missing_start_rows$start_station_id <- NA

for (i in 1:nrow(missing_start_rows)) {
  lat <- missing_start_rows$start_lat[i]
  lng <- missing_start_rows$start_lng[i]
  
  matching_row <- lookup_table_start[lookup_table_start$start_lat == lat & lookup_table_start$start_lng == lng, ]
  
  if (nrow(matching_row) > 0) {
    missing_start_rows$start_station_name[i] <- matching_row$start_station_name[1]
    missing_start_rows$start_station_id[i] <- matching_row$start_station_id[1]
  }
}

# Loop through missing_end_rows
missing_end_rows$end_station_name <- NA
missing_end_rows$end_station_id <- NA

for (i in 1:nrow(missing_end_rows)) {
  lat <- missing_end_rows$end_lat[i]
  lng <- missing_end_rows$end_lng[i]
  
  matching_row <- lookup_table_end[lookup_table_end$end_lat == lat & lookup_table_end$end_lng == lng, ]
  
  if (nrow(matching_row) > 0) {
    missing_end_rows$end_station_name[i] <- matching_row$end_station_name[1]
    missing_end_rows$end_station_id[i] <- matching_row$end_station_id[1]
  }
}

rm(lookup_table)
# View the resulting data frame
missing_data

#-----------------checking how many stations got fixed---------------------------
#----------final count to compare with missing_station_name_ending_counts---

# Count missing values in start_station_name
missing_start_counts <- station_fixed %>%
  filter(is.na(start_station_name) | start_station_name == "") %>%
  summarise(count = n(), station_type = "start_station_name")

# Count missing values in end_station_name
missing_end_counts <- station_fixed %>%
  filter(is.na(end_station_name) | end_station_name == "") %>%
  summarise(count = n(), station_type = "end_station_name")

# Count missing values in start_station_id
missing_start_id_counts <- station_fixed %>%
  filter(is.na(start_station_id) | start_station_id == "") %>%
  summarise(count = n(), station_type = "start_station_id")

# Count missing values in end_station_id
missing_end_id_counts <- station_fixed %>%
  filter(is.na(end_station_id) | end_station_id == "") %>%
  summarise(count = n(), station_type = "end_station_id")

# Combine all the counts into a single data frame
missing_station_name_ending_counts <- bind_rows(missing_start_counts, missing_end_counts, missing_start_id_counts, missing_end_id_counts)

# Remove the individual count data frames
rm(missing_start_counts, missing_end_counts, missing_start_id_counts, missing_end_id_counts)

#---cleanup---

rm(missing_station_name_ending_counts)

#---gives analysis of how many stations there are, what there names are and how often they are referenced---

# Get unique start station names and counts
start_station_counts <- station_fixing %>%
  filter(!(is.na(start_station_name) | start_station_name == "")) %>%
  group_by(start_station_name) %>%
  summarise(count = n())

# Get unique end station names and counts
end_station_counts <- station_fixing %>%
  filter(!(is.na(end_station_name) | end_station_name == "")) %>%
  group_by(end_station_name) %>%
  summarise(count = n())

# Count the total unique stations
total_station_count <- nrow(lookup_table)

# Print the total stations used
cat("Total stations used: ", total_station_count, "\n")

# Print the station count for each start station
print(start_station_counts)

# Print the station count for each end station
print(end_station_counts)

# Cleans up work space
rm(unique_station_start, unique_station_end)


#--cleanup--

rm(station_count,unique_station_names)
---------------------------------------------------------------------

   
    # Creates a stacked bar plot where each bar represents a start station
    # and the bars are stacked base on count or frequency of each user membership type
    top_stations_start <- unique_data %>%
      filter(!is.na(start_station_name)) %>%
      group_by(start_station_name) %>%
      summarise(count = n()) %>%
      top_n(30, count) %>%
      arrange(desc(count))
    
    ggplot(unique_data %>% filter(start_station_name %in% top_stations$start_station_name), aes(x = start_station_name, fill = member_casual)) +
      geom_bar() +
      labs(x = "Start Station", y = "Count", fill = "Member Type") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels if needed
  #cleanup for stacked bar plot
    rm(top_stations_start)