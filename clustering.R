" 
  Code to perform the clustering of bike usage data using K-Means. Adapted from the code written by Carlos Amaral.
"

library(tidyverse)
library(stringr)

#Read data
all <- read_rds("C:/MSc Materials/Dissertation/dublin_bikes-master/saved_data_frames/db_all_data.rds") 
#old <- read_rds("C:/MSc Materials/Dissertation/dublin_bikes-master/saved_data_frames/db_all_data_old.rds") 

#all<-rbind(old, all)
df <- as.tibble(all)

prep_df <- df %>% 
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

########################## Data sanitisation ###############################
"
Function to fix na values in columns
"

fix_na <- function(column){
  # Iterate through rows
  for(i in 1:length(column)){
    # Apply different result if it is first row
    if(i == 1 & is.na(column[i])){
      # If first row is NA, we need to check the next non NA entry and take that
      j = i+1
      while( is.na(column[j]) ) j=j+1
      column[i] <- column[j]
      }
    else{
      # Else replace the NA by the previous value
      ifelse(is.na(column[i]), column[i] <- column[i-1], column[i] <- column[i])
    }
    
  }
  return(column)
} 

prep_df <- as.tibble(lapply(prep_df, fix_na))

########################## Label Adjustment ###################
"A series of operations to make the labels in the graph look pretty "
# Create levels for the times of the day
time_lvl_df <- df %>%
  select(Time) %>%
  distinct() %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract( str_extract(Time, ":\\d+:"), "\\d+") )
  ) %>%
  arrange(t_hour,t_min)

# Vector with the levels for time i.e. order time from 00:00 to 23:50
time_lvls <- time_lvl_df$Time

# Support variables to capture time breaks for the plots
time_breaks <- time_lvl_df %>%
  filter(t_min == 0)
time_breaks <- time_breaks$Time

break_labels <- str_extract(time_breaks, "\\d+:\\d+")
########################## k-means plot for prep df ################
# k-means fit the data
n <- nrow(prep_df)
wss <- rep(0,10)
wss[1] <- (n-1) * sum(sapply(prep_df,var))

# Running K-Means with cluster numbers 2 to 10
for (i in 2:10)  wss[i] <- sum(
  kmeans(prep_df, centers=i)$withinss
)

png(filename = "wss.png")

# TWSS Graph
cl_exam <- plot(1:10, wss, type="b", xlab="Number of Clusters",
                ylab="Within groups sum of squares")
dev.off()

# 4-means clustering
kfit <- kmeans(prep_df, centers = 4)

# Specific colours for the clusters
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cluster_colours <- gg_color_hue(4)

# Change format of data for plotting
k_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
all_centroid_plot <- k_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Clustering for all stations")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(
    breaks = k_centers$cluster,
    values = cluster_colours
  )

all_centroid_plot

ggsave("plots/clustering/Clustering for all stations.png", all_centroid_plot)

########################## Comparison between Weekday / Weekend ####################
"######## Filter out Weekends ##############"
week_df <- df %>% 
  filter(
    Weekday %in% c("Mon", "Tue", "Wed", "Thur")
  ) %>%
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

week_df <- as.tibble(lapply(week_df, fix_na))

# k-means fit the data
n <- nrow(week_df)
wss <- rep(0,15)
wss[1] <- (n-1) * sum(sapply(week_df,var))

# 
for (i in 2:10)  wss[i] <- sum(
  kmeans(week_df, centers=i)$withinss
)

# Plot wss for each number of clusters and use elbow method
plot(1:10, wss, type="b", xlab="Number of Clusters",
                ylab="Within groups sum of squares")

# 4-means clustering
kfit <- kmeans(week_df, centers = 4)

# Change format of data for plotting
week_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
week_centroid_plot <- week_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Weekday clusters (Mon - Thu)")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(
    breaks = week_centers$cluster,
    values = cluster_colours
  )
week_centroid_plot
ggsave("plots/clustering/weekday clustering.png", week_centroid_plot)  

"########### Filter out Weekdays ##########"
end_df <- df %>% 
  filter(
    Weekday %in% c("Fri", "Sat", "Sun")
  ) %>%
  mutate(
    Time = str_replace_all(Time, ":", "_")
  ) %>%
  select(Number, Date,  Time, Available_stands) %>%
  group_by(Number, Date) %>%
  spread(key = Time, value = Available_stands) %>%
  ungroup() %>%
  select(-Number, -Date)

end_df <- as.tibble(lapply(end_df, fix_na))

# k-means fit the data
n <- nrow(end_df)
wss <- rep(0,15)
wss[1] <- (n-1) * sum(sapply(end_df,var))

# Subsequent values can be found using this loop, note '15' is arbitrary 
for (i in 2:15)  wss[i] <- sum(
  kmeans(end_df, centers=i)$withinss
)

# Plot wss for each number of clusters and use elbow method
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# 4-means clustering
kfit <- kmeans(end_df, centers = 4)

# Change format of data for plotting
end_centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = as.factor(row_number())) %>%
  gather(key = time, value = available_stands, -cluster) %>%
  mutate( 
    time = str_replace_all(time, "_", ":"),
    time = factor(time, levels = time_lvls)
  )

## Plot the data
end_centroid_plot <- end_centers %>% 
  ggplot(aes(x = time, y = available_stands, colour = cluster, group = cluster)) +
  geom_line() +
  scale_x_discrete(
    breaks = time_breaks,
    labels = break_labels
  ) + 
  xlab("Hour of day") +
  ylab("Available stands") +
  ggtitle("Weekends clusters (Fri - Sun)")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(
    breaks = end_centers$cluster,
    values = cluster_colours
  )
end_centroid_plot
ggsave("plots/clustering/weekend clustering.png", end_centroid_plot)  


########################## Aggregate Clustering results with data #################
" Add clustering results back to the original data frame "
# Create df with the centroid
centers <- as.tibble(kfit$centers) %>%
  mutate(cluster = row_number()) %>%
  gather((`0_00_00`:`9_50_00`), key = "time", value = "available_stands") %>%
  group_by(cluster) %>%
  summarise(
    sum_of_stands = sum(available_stands)
  ) %>% 
  ungroup()

# Compare each station with each centroid
cluster_df <- df %>%  
  filter(Name != "CHATHAM STREET") %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract( str_extract(Time, ":\\d+:"), "\\d+") )
  ) %>%
  group_by(Number, Name, Time, t_hour, t_min) %>%
  summarise(stands = mean(Available_stands)) %>%
  arrange(t_hour, t_min) %>%
  ungroup() %>%
  select(-t_hour, -t_min) %>%
  arrange(Number) %>%
  mutate(
    cluster = "none"
  )

# Loop through each station and find the cluster which is closer to it
for(i in 1:102){
  # Get the sum of stands for the stands
  st <- as.numeric(cluster_df %>%
    filter(Number == i) %>%
    summarise( 
      sum_of_stands = sum(stands)
    ))

  # Subtract it from centroid sum of stands and find minimal one
  min_diff <- centers %>%
    mutate(
      station_sum = st,
      diff = abs(sum_of_stands - station_sum)
    ) %>%
    arrange(diff) %>%
    filter(row_number() == 1)
  
  # add result back to the df
  cluster_df <- cluster_df %>%
    mutate(
      cluster = if_else(
        Number == i,
        as.character(min_diff$cluster),
        cluster
      )
    )
}

cdf <- cluster_df %>%
  group_by(Number, Name, cluster) %>%
  summarise() %>% ungroup()
write_rds(cdf, "db_clustered_stations.rds")

############################### Plot the clusters on a map ################################
library(ggmap)
geo <- read_csv("geo_data/db_geo.csv")

# Drop unnecessary columns for geo
geo <- geo %>%
  select(Number, Latitude, Longitude)

# Do the join
plot_df <- cluster_df %>%
  left_join(geo, by = "Number") 
plot_df <- plot_df %>%
  group_by(Number, Name, Latitude, Longitude, cluster) %>%
  summarise() %>% 
  ungroup() %>%
  arrange(cluster)

# Map of dublin
dub_map <- ggmap(
  get_googlemap(
    center = c(-6.270,53.345),
    scale = 2,
    zoom = 13,
    size = c(540,400)
  )
) +
  coord_fixed(ratio = 1.3)

cluster_plot <- dub_map + 
  geom_point(
    data = plot_df,
    aes(
      x = Longitude,
      y = Latitude,
      colour = cluster
    ),
    size = 3
  ) +
  scale_colour_manual(
    breaks = plot_df$cluster,
    values = cluster_colours
  )
cluster_plot
ggsave("plots/clustering/map_clusters.png", cluster_plot)
