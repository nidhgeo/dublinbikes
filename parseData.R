" 
  Description:
    Code to read and parse Dublin Bikes usage data. Adapted from the code by Carlos Amaral.

"

library(tidyverse)
library(jsonlite)
library(lubridate)

################################ Read in JSON and CSV files #################################

# Get all file names in the directory
file_names <- dir()

# Read in all json files, flattening where appropriate
all_df <- do.call(rbind, lapply(file_names, fromJSON, flatten = TRUE))


#Get all file names in the directory
file_names <- dir()
# Skip the first one as is problematic
file_names <- file_names[-1]

# Read all files into data frame
df <- do.call(rbind, lapply(file_names, read_csv))

#COmbine the old and new data
all_df<-rbind(df, all_df)

# Save ithe data as rds
write_rds(all_df, "db_raw_new_data.rds")

################################ Support Functions ###################################

# Function to calculate number of checked in bikes
# Args: diff <- difference in # of bikes from previous period to current period
c_check_in <- function(diff){
  # Check if there was a check in
  if(diff > 0){
    return (diff)
  }
  # else return 0 as there was no check in
  else{
    return (0)
  }
}

# Function to calculate number of checked out bikes
# Args: diff <- difference in # of bikes from previous period to current period
c_check_out <- function(diff){
  # Check if there was a check out
  if(diff < 0){
    return (abs(diff))
  }
  # else return 0 as there was no check out
  else{
    return (0)
  }
}

################################## Parse the data #################################
df <- read_rds("db_raw_new_data.rds")

# Separate Duplicates
dup <- duplicated(df)
dup <- df %>%
  mutate(duplicate = dup) %>%
  filter(dup == TRUE)

df <- distinct(df)

# Calculate difference in number of bikes between periods
df <- df %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = as_datetime(last_update/1000, tz = "GMT"),
    Year = year(last_update),
    Month = month(last_update),
    Day = day(last_update),
    Hour = as.character(hour(last_update)),
    Min = minute(last_update),
    Sec = "00"
  ) %>%  
  # Standardise minutes i.e. group them in 10 minute slots and make new time
  mutate(
    Min = ifelse(Min < 10, "00",
                 ifelse(Min < 20, "10",
                        ifelse(Min < 30, "20",
                               ifelse(Min < 40, "30",
                                      ifelse(Min < 50, "40",
                                             ifelse(Min < 60, "50",
                                                    NA)))))
    ),
    Date = ymd(paste(Year, Month, Day, sep = "-")),
    Time = paste(Hour, Min, Sec, sep = ":"),
    Weekday = weekdays(Date, abbreviate = TRUE)
  ) %>%
  # Sort in order so we can calculate the differences
  group_by(number) %>%
  arrange(Year, Month, Day, Hour, Min, Sec) %>%
  # Calculate differences
  mutate(
    prev_period_diff = 
      available_bike_stands - lag(available_bike_stands, default = available_bike_stands[1])
  ) %>%
  # Apply functions to determine checked in/out 
  rowwise() %>%
  mutate(
    check_in = c_check_in(prev_period_diff),
    check_out = c_check_out(prev_period_diff)
  ) %>%
  # Group results in slots of 10 minutes
  group_by(number, name, address, Date, Time, Weekday) %>%
  # Add up results
  summarise(
    bike_stands = min(bike_stands),
    prev_period_diff = sum(prev_period_diff),
    check_in = sum(check_in),
    check_out = sum(check_out),
    available_stands = last(available_bike_stands)
  ) %>%
  ungroup()

# Rename columns
df <- df %>%
  rename(
    Number = number,
    Name = name,
    Address = address,
    Bike_stands = bike_stands,
    Prev_period_diff = prev_period_diff,
    Check_in = check_in,
    Check_out = check_out,
    Available_stands = available_stands
  )

# Add factor level information to days in the week
days_level <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
df <- df %>%
  mutate(Weekday = factor(Weekday, levels = days_level))

# Write output to rds file so code doesn't have to be re-run
write_rds(df, "db_all_data.rds")

