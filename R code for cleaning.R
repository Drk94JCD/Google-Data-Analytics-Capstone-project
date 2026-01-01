# =====================================================
# Cyclistic Bike-Share Data Cleaning Script
# Datasets: Divvy_Trips_2019_Q1.csv and Divvy_Trips_2020_Q1.csv
# Goal: Clean and combine data to compare casual vs. member usage
# =====================================================

# 1. Load required packages -------------------------------------------------
library(tidyverse)   # for data manipulation (dplyr, tidyr, etc.)
library(lubridate)   # for handling dates and times
library(readr)       # for reading CSV files

# 2. Set working directory (optional - adjust to your path) ---------------
# setwd("C:/Users/jdiego/OneDrive - CSCU/Documents/lmms/projects/Data_Cyclistic bike-share")

# 3. Import the raw datasets ------------------------------------------------
tripdata_2019_q1 <- read_csv("raw_data/Divvy_Trips_2019_Q1.csv")
tripdata_2020_q1 <- read_csv("raw_data/Divvy_Trips_2020_Q1.csv")

# 4. Standardize column names and user types --------------------------------
# 2019 Q1 columns: trip_id, start_time, end_time, bikeid, tripduration, 
#                  from_station_id, from_station_name, to_station_id, 
#                  to_station_name, usertype, gender, birthyear

# 2020 Q1 columns: ride_id, started_at, ended_at, start_station_name, 
#                  end_station_name, start_station_id, end_station_id,
#                  member_casual, rideable_type

# Rename and select comparable columns
tripdata_2019_clean <- tripdata_2019_q1 %>%
  rename(
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    start_station_name = from_station_name,
    end_station_name = to_station_name,
    member_casual = usertype
  ) %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual")) %>%
  select(ride_id, started_at, ended_at, start_station_name, 
         end_station_name, member_casual)

tripdata_2020_clean <- tripdata_2020_q1 %>%
  select(ride_id, started_at, ended_at, start_station_name, 
         end_station_name, member_casual)

# 5. Combine the two datasets ------------------------------------------------
trips_combined <- bind_rows(tripdata_2019_clean, tripdata_2020_clean)

# 6. Clean and create new variables -----------------------------------------
trips_clean <- trips_combined %>%
  # Convert to datetime if not already
  mutate(
    started_at = as_datetime(started_at),
    ended_at = as_datetime(ended_at)
  ) %>%
  # Calculate ride length in seconds
  mutate(
    ride_length_sec = as.numeric(difftime(ended_at, started_at, units = "secs")),
    ride_length_min = ride_length_sec / 60
  ) %>%
  # Extract day of week (1 = Sunday, 7 = Saturday to match your Sheets format)
  mutate(
    day_of_week = wday(started_at, week_start = 7),  # week_start=7 makes Sunday=1
    day_name = weekdays(started_at)
  ) %>%
  # Extract additional useful fields
  mutate(
    start_hour = hour(started_at),
    date = as_date(started_at),
    month = month(started_at, label = TRUE)
  )

# 7. Remove invalid rides ----------------------------------------------------
trips_clean <- trips_clean %>%
  filter(
    ride_length_sec > 60,                 # Remove rides ≤ 1 minute (likely false starts/docking errors)
    ride_length_sec < 86400,              # Remove rides ≥ 24 hours (likely lost/stolen/maintenance)
    !is.na(started_at), !is.na(ended_at), # Remove missing times
    !is.na(member_casual)
  )

# 8. Final checks ------------------------------------------------------------
glimpse(trips_clean)
summary(trips_clean$ride_length_min)

# Check user type distribution
trips_clean %>% count(member_casual)

# Check for any remaining negative ride lengths
trips_clean %>% filter(ride_length_sec < 0) %>% nrow()  # Should be 0

# 9. Save cleaned dataset ----------------------------------------------------
write_csv(trips_clean, "processed_data/cyclistic_trips_2019_2020_q1_cleaned.csv")

# Optional: Save for quick reload later
saveRDS(trips_clean, "processed_data/cyclistic_trips_cleaned.rds")