"
  Code to perform Data Analysis and Exploration. Adapted from the code written by Carlos Amaral. 
"

library(tidyverse)
library(lubridate)
library(stringr)
library(ggmap)
library(dplyr)
library(ggplot2)
library(stringr)

################################# Exploratory Analysis #####################################

# Read previously processed data
df <- read_rds("C:/MSc Materials/Dissertation/dublin_bikes-master/saved_data_frames/db_all_data.rds")

#old <- read_rds("C:/MSc Materials/Dissertation/dublin_bikes-master/saved_data_frames/db_all_data_old.rds") 
#%>%  filter(Date >= "2016-10-14" & Date <= "2017-10-31")

#df<-rbind(old, df)
dim(df)

df <- as.tibble(df%>%  filter(Date <= "2017-10-20"))

summary(df$Date)


wInfo <- read_csv("M2_weather.csv")
wInfo <- distinct(wInfo)

# Calculate difference in number of bikes between periods
wInfo <- wInfo %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = parse_date_time(datetime, orders="ymd HMS", tz = "UTC"),
    Year = year(last_update),
    Month = month(last_update),
    Day = day(last_update),
    Hour = as.numeric(hour(last_update)),
    Min = minute(last_update),Sec = "00",
    Date = ymd(paste(Year, Month, Day, sep = "-")),
    Time = paste(Hour, Min, Sec, sep = ":")
  )

df <- df %>% mutate(Hour=as.numeric(str_extract(df$Time, "^\\d{1,2}")))
df <- df %>%
  left_join(wInfo, by=c("Date", "Hour"))

#Holiday Data
holidays <- read_csv("holiday16-17.csv")

holidays <- holidays %>%
  mutate(
    
    #Date = parse_date_time(Date, orders="ymd"),
    #Year = year(Date),
    #Month = month(Date),
    #Day = day(Date),
    #Date = ymd(paste(Year, Month, Day, sep = "-"))
    Date=ymd(Date)
  )



df <- df %>%
  left_join(holidays, by=c("Date"))

write_csv(df, "df_exploration.csv")

df <- df %>%
  mutate(holiday=ifelse(df$`Holiday_Name`!='NA', 'T', 'F'))

df$holiday <- ifelse(is.na(df$holiday), 'F', 'T')


# Airtemperature data
summary(df$AirTemperature)
df$AirTemperatureInd <- with(df, cut(AirTemperature, 
                                breaks=quantile(AirTemperature, probs=seq(0,1, by=0.33), na.rm=TRUE), 
                                include.lowest=TRUE))

  
#df$AirTemperatureInd <-   factor(df$AirTemperatureInd, levels=c("low","medium","high") )

df$AirTemperatureInd <- factor(as.numeric(df$AirTemperatureInd))
df$AirTemperatureInd <- revalue(df$AirTemperatureInd, c("1"="Low", "2"="Medium", "3" = "High"))

summary(df$AirTemperatureInd)

#Windspeed data

summary(df$WindSpeed)

df$windSpeedInd <- with(df, cut(WindSpeed, 
                                     breaks=quantile(WindSpeed, probs=seq(0,1, by=0.33), na.rm=TRUE), 
                                     include.lowest=TRUE))


df$windSpeedInd <- factor(as.numeric(df$windSpeedInd))
df$windSpeedInd <- revalue(df$windSpeedInd, c("1"="Low", "2"="Medium", "3" = "High"))

summary(df$windSpeedInd)



# Look at the missing dates
df %>% 
  group_by(y = year(Date), m = month(Date), d = day(Date)) %>%
  summarise( n() ) %>%
  arrange(y, m, d) %>%
  View()


# Filter out data for top 10 most and least used stations
top_activity <- df %>%
  mutate(act = Check_in + Check_out) %>%
  group_by(Address) %>%
  summarise(tot_act = sum(act)) %>%
  top_n(10, tot_act)
top_activity

bot_activity <- df %>%
  mutate(act = Check_in + Check_out) %>%
  group_by(Address) %>%
  summarise(tot_act = sum(act)) %>%
  top_n(-10, tot_act)
bot_activity


df %>%
  group_by(Name) %>%
  summarise(tot_check_in = sum(Check_in)) %>%
  top_n(10, tot_check_in) %>%
  View()

# Average Usage per day
avg_usage_day <- df %>%
  group_by(Number, Name, Weekday) %>%
  summarise(
    avg_cin = mean(Check_in),
    avg_cout = mean(Check_out)
  )

# Usage per day
usage_day <- df %>%
  mutate( act = Check_in + Check_out) %>%
  group_by(Number, Name, Weekday) %>%
  summarise(
    tot_act = sum(act)
  )

# Usage per hour
usage_hour <- df %>%  filter(Number==5) %>%
  mutate( act = Check_in + Check_out) %>%
  group_by(Hour) %>%
  summarise(
    tot_act = mean(act)
  )

# Usage per hour - holiday
usage_hourly_holiday <- df %>%
  filter(holiday=='T') %>%
  mutate( act = Check_in + Check_out) %>%
  group_by(Hour) %>%
  summarise(
    mean_tot_act = mean(act)
  )

usage_hourly_nonHoliday <- df %>%
  filter(holiday=='F') %>%
  mutate( act = Check_in + Check_out) %>%
  group_by( Hour) %>%
  summarise(
    mean_tot_act = mean(act)
  )

usage_temperature <- df %>%
  mutate( act = Check_in + Check_out) %>%
  group_by(Number, Name, AirTemperatureInd) %>%
  summarise(
    mean_tot_act = mean(act)
  )

usage_windspeed <- df %>%
  mutate( act = Check_in + Check_out) %>%
  group_by(Number, Name, windSpeedInd) %>%
  summarise(
    mean_tot_act = mean(act)
  )


# Boxplots to understand the avg distribution
avg_usage_day %>%
  ggplot(aes(Weekday, avg_cin)) +
  geom_boxplot()

avg_usage_day %>%
  ggplot(aes(Weekday, avg_cout)) +
  geom_boxplot()

usage_day %>%
  ggplot(aes(Weekday, tot_act)) +
  geom_boxplot() +
  ylab("Activity")

usage_hourly_holiday %>%
  ggplot(aes(Hour, mean_tot_act)) +
  geom_bar(stat="identity") +
  ylab("Holiday Mean Total Activity")

usage_hourly_nonHoliday %>%
  ggplot(aes(Hour, mean_tot_act)) +
  geom_bar(stat="identity") +
  ylab("Non-Holiday Mean Total Activity")

subset(usage_temperature, !is.na(AirTemperatureInd)) %>%
  ggplot(aes(AirTemperatureInd, mean_tot_act)) +
  geom_bar(stat="identity", fill="#3762a8") +
  ylab("Mean Total Activity") 
  
  

subset(usage_windspeed, !is.na(windSpeedInd)) %>%
  ggplot(aes(windSpeedInd, mean_tot_act)) +
  geom_bar(stat="identity", fill="#3762a8") +
  ylab("Mean Total Activity")

usage_hour %>%
  ggplot(aes(Hour, tot_act)) +
  geom_bar(stat="identity") +
  ylab("Total Activity")


# Examine usage in 4 periods, morning, afternoon, evening , night
day_periods <- df %>%
  mutate(
    Hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    Period = ifelse(Hour >= 5 & Hour < 12, "Morning",
              ifelse(Hour >= 12 & Hour < 19, "Afternoon",
               ifelse(Hour >= 19, "Night", 
                "Late Night")))
  ) %>%
  group_by(Number, Name, Period, Weekday) %>%
  summarise(
    tot_in = sum(Check_in),
    tot_out = sum(Check_out),
    avg_in = mean(Check_in),
    avg_out = mean(Check_out)
  )


day_periods$Period <- factor(day_periods$Period, levels = c("Late Night",  "Night", "Afternoon", "Morning" ))

day_periods %>%
  ggplot(aes(Period)) +
  geom_col(aes(y = tot_in), width = 0.7, fill = "blue") +
  geom_col(aes(y = tot_out), width = 0.5, fill = "red") +
  scale_fill_manual(values = c("blue"))+
  coord_flip()

################################# Locations of top/bottom stations ####################################
# Data frame with stations of interest (i.e busiest and least busy)
act_df <- df %>%
  inner_join(rbind(top_activity, bot_activity), by = "Address") %>%
  group_by(Number, Address, tot_act) %>%
  summarise() %>%
  ungroup() %>%
  mutate(
    rank = if_else(tot_act > 50000, "Most Active", "Least Active")
  )

geo_df <- read_csv("geo_data/db_geo.csv")
act_df <- act_df %>%
  left_join(geo_df)

dub_map <- ggmap(
  get_googlemap(
    center = c(-6.270,53.345),
    scale = 2,
    zoom = 13,
    size = c(540,400)
  )
) +
  coord_fixed(ratio = 1.3)

top_bot_act <- dub_map +
  geom_point(
    data = act_df,
    aes(Longitude, Latitude, colour = rank),
    size = 5,
    alpha = 0.8
  ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
  ) +
  scale_colour_manual(
    breaks = rank,
    values = c("#FF0000", "#3399FF")
  )
ggsave("plots/geoplots/top_bottom_activity.png", top_bot_act)

################################# Seasonality effects ###########################
library(reshape)
# Investigate wheter seasonality has any effect on usage
season_df <- df %>%
  mutate(
    season = factor(
      if_else(month(Date) == 11 |month(Date) == 12 | month(Date) == 1, "Winter",
              if_else(month(Date) == 2 |month(Date) == 3 | month(Date) == 4, "Spring",
                      if_else(month(Date) == 5 |month(Date) == 6 |month(Date) == 7, "Summer",
                              "Autumn"))),
      levels = c("Winter", "Spring", "Summer", "Autumn") 
    ),
    activity = Check_in + Check_out
  ) %>%
  select(Weekday, season, activity) %>%
  melt(id = c("Weekday", "season")) %>%
  group_by(Weekday, season) %>%
  summarise( 
    activity = mean(value)
  ) %>%
  ungroup()

summary(season_df$season)

season_plot <- season_df %>%
  ggplot(aes(season, activity, fill=season)) +
  geom_col() +
  facet_wrap(~ Weekday, nrow = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
season_plot

ggsave("plots/season_lplot.png", season_plot)


