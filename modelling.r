"
  Code to perform Regression and Classification modelling on Dublin Bikes
  usage data. 
"
####################### Import packages set seed ###################################
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(Metrics)
library(corrplot)
library(grid)
library(gridExtra)
library(gbm)
library(stringr)
library(lubridate)
library(pls)
library(leaps)
library(bst)
library(MASS)
library(dplyr)
library(fitdistrplus)
library(logspline)
library(e1071) 
# set a seed so that we can reproduce results
set.seed(3482)

###################### Analyse top/bottom stations in each cluster ###################
clusters <- read_rds("db_clustered_stations.rds")
all <- read_rds("db_all_data.rds") 
#%>%  filter(Date >= "2016-10-14" & Date <= "2017-10-31")

#old <- read_rds("db_all_data_old.rds") 
#%>%  filter(Date >= "2016-10-14" & Date <= "2017-10-31")

#head(all)

#all<-rbind(old, all)
dim(all)

summary(all$Date)
df_backup<-df

all_clust <- df %>%
  left_join(clusters, by = "Number") %>%
  mutate(cluster = factor(cluster)) %>%
  select(-Name.y, Name = Name.x)

# Find the most active station per cluster
top_stations <- all_clust %>%
  mutate( activity = Check_in + Check_out) %>%
  group_by(Number, cluster) %>%
  summarise( tot_act = sum(activity)) %>%
  ungroup() %>%
  group_by(cluster) %>%
  top_n(1, tot_act) %>%
  ungroup() %>%
  arrange(activity)

# find the least active station per cluster
bot_stations <- all_clust %>%
  mutate( activity = Check_in + Check_out) %>%
  group_by(Number, cluster) %>%
  summarise( tot_act = sum(activity)) %>%
  ungroup() %>%
  group_by(cluster) %>%
  top_n(-1, tot_act) %>%
  ungroup()

# gets the station numbers for most active and least active into a list
selected <- rbind(top_stations, bot_stations) %>%
  pull(Number)

# Randomly select 3 stations in each cluster for the random forest modelling
for(clust_num in 1:4){
  n <- all_clust %>%
    filter(cluster == clust_num) %>%
    filter(!(Number %in% selected)) %>%
    group_by(Number, cluster) %>%
    summarise() %>%
    ungroup() %>%
    sample_n(3) %>% 
    pull(Number)
  selected <- append(selected, n)
}
####################### Prepare the data for random forest #################################
# filter the full dataset based on the selection above
df <- all_clust %>%
  filter(Number %in% selected)


selected
# Group time into 48 factors and categorize the seasons
time_df2 <- df %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract( str_extract(Time, ":\\d+:"), "\\d+")),
    Time = as.factor(if_else( 
      t_min < 30, 
      if_else(
        t_hour < 10, 
        paste(paste("0", t_hour, sep = ""), "00", sep=":"), 
        paste(t_hour, "00", sep=":")
      ),
      if_else(
        t_hour < 10, 
        paste(paste("0", t_hour, sep = ""), "30", sep=":"), 
        paste(t_hour, "30", sep=":")
      )
    )),
    #Name = as.factor(Name),
    season = as.factor(
              if_else(month(Date) == 11 |month(Date) == 12 | month(Date) == 1, "Winter",
                if_else(month(Date) == 2 |month(Date) == 3 | month(Date) == 4, "Spring",
                  if_else(month(Date) == 5 |month(Date) == 6 |month(Date) == 7, "Summer",
                    "Autumn")))
              ),
    av_bikes = Bike_stands - Available_stands
  ) %>%
  group_by(
    Number, Name, Address, Bike_stands, Date, season, Weekday, Time
  ) %>%
  summarise(
    av_bikes = round(mean(av_bikes)),
    cluster = first(cluster)
  ) %>%
  mutate( 
    cluster = as.factor(cluster), 
    prev_bike_num = lag(av_bikes), # number of bikes in previous time period
    prev_bike_num = if_else(is.na(prev_bike_num), av_bikes, prev_bike_num),
    prev_bike_num2 = lag(av_bikes, k=3), # number of bikes in previous time period
    prev_bike_num2 = if_else(is.na(prev_bike_num2), av_bikes, prev_bike_num2)
  ) %>% 
  ungroup()



#Calculating the bike numbers from precious weeks
for(i in 1:nrow(time_df2)) {
  currDate <- time_df2[i,"Date"]
  currTime <- time_df2[i,"Time"]
  timeVal <- as.data.frame(currTime)[[1]]
  stationNumber <- time_df2[i,"Number"][[1]]
  prevWeekDate <- currDate - 7
  prevWeekDate2 <- currDate - 14
  prevWeekDate3 <- currDate - 21
  prevWeekDate4 <- currDate - 28
  rowsFromPrevWeek <- filter(time_df2, Number == stationNumber & Date==prevWeekDate[[1]] & Time %in% timeVal)
  rowsFromPrevWeek2 <- filter(time_df2, Number == stationNumber & Date==prevWeekDate2[[1]] & Time %in% timeVal)
  rowsFromPrevWeek3 <- filter(time_df2, Number == stationNumber & Date==prevWeekDate3[[1]] & Time %in% timeVal)
  rowsFromPrevWeek4 <- filter(time_df2, Number == stationNumber & Date==prevWeekDate4[[1]] & Time %in% timeVal)
  if(nrow(rowsFromPrevWeek) > 0) {
    avgAVBikes <- mean(rowsFromPrevWeek[,"av_bikes"][[1]], na.rm = T)
    time_df2[i,"prevWeek_bike_num"] <- avgAVBikes 
  }  
 # else {
 #   time_df2[i,"prevWeek_bike_num"] = lag(time_df2[i, "av_bikes"])
 # }
  if(nrow(rowsFromPrevWeek2) > 0) {
    avgAVBikes2 <- mean(rowsFromPrevWeek2[,"av_bikes"][[1]], na.rm = T)
    time_df2[i,"prevWeek_bike_num2"] <- avgAVBikes2
  }  
 # else {
 #   time_df2[i,"prevWeek_bike_num2"] = lag(time_df2[i, "av_bikes"])
 # }
  if(nrow(rowsFromPrevWeek3) > 0) {
    avgAVBikes3 <- mean(rowsFromPrevWeek3[,"av_bikes"][[1]], na.rm = T)
    time_df2[i,"prevWeek_bike_num3"] <- avgAVBikes3 
  }  
 # else {
 #   time_df2[i,"prevWeek_bike_num3"] = lag(time_df2[i, "av_bikes"])
 # }
  if(nrow(rowsFromPrevWeek4) > 0) {
    avgAVBikes4 <- mean(rowsFromPrevWeek4[,"av_bikes"][[1]], na.rm = T)
    time_df2[i,"prevWeek_bike_num4"] <- avgAVBikes4 
  }  
 # else {
 #  time_df2[i,"prevWeek_bike_num4"] = lag(time_df2[i, "av_bikes"])
 # }
 # if(is.na(time_df2[i,"prevWeek_bike_num"])){
 #   time_df2[i,"prevWeek_bike_num"] = time_df2[i, "av_bikes"]
 # } 
}

#Saving the data frame
write_rds(time_df2, "time_df2_withprevweeks.rds") 

head(wInfo)
head(rf_df)

#rf_df <- rf_df %>% mutate(Hour=as.numeric(str_extract(rf_df$Time, "^\\d{1,2}")))
#df2<-NULL
#df2 <- merge(x=rf_df, y=wInfo, by=c("Date", "Hour"), all.x=TRUE)
#head(df2)

#df2<-df2%>%
#  mutate(
 #   av_perc= (av_bikes/Bike_stands)*100,
 #   av_ind = ifelse(av_perc<=10,'low','high')
#  )

# Create training and test samples
#n = floor(nrow(rf_df)/ 10)
#lb = 7*n +1
#ub = 10*n
#subset = lb:ub

# DF containing geographical info
geo <- read_csv("geo_data/db_geo.csv")

# Join the geographical info to the original dataset
time_df2 <- time_df2 %>%
  left_join(geo, by = "Number") %>%
  select(-Name.y, -Address.y, Name = Name.x, Address = Address.x)

#Reading the weather info from M2 weather buoy
wInfo <- read_csv("M2_weather.csv")

wInfo <- distinct(wInfo)

wInfo <- wInfo %>%
  # Convert POSIXct to date and split into each col
  mutate(
    last_update = parse_date_time(datetime, orders="mdy HM", tz = "UTC"),
    Year = year(last_update),
    Month = month(last_update),
    Day = day(last_update),
    Hour = as.numeric(hour(last_update)),
    #Min = minute(last_update),Sec = "00",
    DateNew = ymd(paste(Year, Month, Day, sep = "-"))
    #Time = paste(Hour, Min, Sec, sep = ":")
  )

#Imputing the missing values with mean
wInfo$AirTemperature[is.na(wInfo$AirTemperature)]=mean(wInfo$AirTemperature, na.rm = T)
wInfo$WindSpeed[is.na(wInfo$WindSpeed)]=mean(wInfo$WindSpeed, na.rm = T)


time_df2 <- read_rds("time_df2_withprevweeks.rds")
# Join the geographical info to the original dataset
rf_df2 <- time_df2 %>%
  left_join(geo, by = "Number") 
#%>%select(-Name.y, -Address.y, Name = Name.x, Address = Address.x)

#df2 <- merge(x=rf_df2, y=wInfo, by=c("Date", "Hour"), all.x=TRUE)


rf_df2 <- rf_df2 %>% mutate(Hour=as.numeric(str_extract(Time, "^\\d{1,2}")),
                            DateNew=ymd(Date))

write.csv(wInfo, file = "wInfo.csv")
write.csv(rf_df2, file = "rf_df2.csv")

#Joining weather info with bike usage data
df2<-NULL
df2 <- merge(rf_df2, wInfo, all.x=T)



#Reading Holiday Data
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


#joining holiday data with the usage-weather data frame
df2 <- df2 %>%
  left_join(holidays, by=c("Date"))

write_csv(df2, "df2_exploration.csv")

#Creating a categorical column for holiday/observance indicator
df2 <- df2 %>%
  mutate(holiday=ifelse(df2$`Holiday_Name`!='NA', 'T', 'F'))

df2$holiday <- ifelse(is.na(df2$holiday), 'F', 'T')



write.csv(df2, file = "df2-test.csv")

#Creating new columns for available bikes percentage
df2<-df2%>%
  mutate(
    av_perc= (av_bikes/Bike_stands)*100,
    av_ind = ifelse(av_perc<=25,'low','high'),
    prevweeks_avg = ((prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4)/4)
  )

df2$av_ind <- factor(df2$av_ind)


#n = floor(nrow(rf_df)/ 10)
#lb = 7*n +1
#ub = 10*n
#subset = lb:ub

write.csv(df2, file = "df2-with_prevweeks_avind.csv")


#df2$WindSpeed[is.na(df2$WindSpeed)]=0
#df2$AirTemperature[is.na(df2$AirTemperature)]=0

# Create training and test samples
n = floor(nrow(df2)/ 4)
set.seed(3482)
indexValues <- sample( nrow(df2),n, replace=T)

train <- df2[-indexValues,]
test <- df2[indexValues,]


#Checking for invalid data
test[test[,"av_bikes"]<0,]
which(train[,"av_bikes"]<0)
which(test[,"av_bikes"]<0)

#Removing invalid rows
train<-train[-c(63107,  63117, 74841),]

newcombined<-rbind(train, test)

which(train[,"season"]=="Summer")
which(test[,"season"]=="Summer")

#Removing row with Summer value to avoid zero variance
test<-test[-16551,]

dim(test)

write.csv(train, file = "train_df2-with_prevweeks_avind.csv")
write.csv(test, file = "test_df2-with_prevweeks_avind.csv")


train<-NULL
train<- read_csv("train_df2-with_prevweeks_avind.csv", col_types = cols(Time.x = col_character()))

test<-NULL
test<- read_csv("test_df2-with_prevweeks_avind.csv", col_types = cols(Time.x = col_character()))



#typeof(prevWeekDate[[1]])
#newDF <- time_df2[, time_df2$Date==prevWeekDate[[1]] & time_df2$Time == (currTime[1,1])]
#time_df2[,time_df2$Time %in% currTime]
#nrow(filter(time_df2, Date == "2016-09-12"))
#time_df2[, "prevWeek_bike_num"]


# Run random forest model and evaluate results
start.time <- Sys.time()
rf <- randomForest(av_bikes ~  Weekday + Time.x + prev_bike_num +
                     cluster + Latitude + Longitude + season + AirTemperature +WindSpeed,
                   data = train, importance = TRUE, ntree = 200, na.action=na.roughfix)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

rf
plot(rf)
importance(rf)
varImpPlot(rf)
write_rds(rf, "saved_data_frames/rf_model_with_weather.rds") # Save rf to avoid running it again



# Predict target data fram and evaluate error
test$pred <- predict(rf, test)
rmsle(test$pred, test$av_bikes)
rmse(test$pred, test$av_bikes)



rf <- read_rds("rf_model_with_weather.rds") # Import model
test$pred <- predict(rf, test)
rmsle(test$pred, test$av_bikes)
rmse(test$pred, test$av_bikes)
######################################## Plot results ############################
# Plot prediction to compare it against actual results
test$pred <- round(test$pred)

rf_err <- rmse(test$pred, test$av_bikes)
rf_err

time_breaks <- test %>%
  filter(str_detect(Time, "\\d{2}:00")) %>%
  group_by(Time) %>%
  summarise() %>%
  pull(Time)

####################################################################################
# Label for the error
err_label <- paste("Error margin:", round(rf_err, 2))

# plot means per stations
base_plot <- test %>%
  group_by(Number, Address, Time) %>%
  summarise(
    mean_av_bikes = mean(av_bikes),
    mean_pred = mean(pred),
    tot_stands = max(Bike_stands)
  ) %>%
  ggplot(aes(Time, mean_av_bikes, group = 1)) +
  theme_minimal() +
  geom_line(aes(y = tot_stands, colour = "Total stands"), linetype = 6) +
  geom_line(aes(colour = "Actual"), size = 1.1) +
  geom_line(aes(y = mean_pred, group = 1, colour = "Predicted"), size = 1.1) +
  geom_ribbon(aes(ymin = mean_pred - rf_err, ymax = mean_pred + rf_err), fill = "grey30", alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) + 
  scale_x_discrete(
    breaks = time_breaks,
    labels = time_breaks
  ) + 
  ggtitle("Random forest prediction and actual number") +
  ylab("Mean available bikes") +
  scale_colour_manual(
    breaks = c("Predicted", "Actual", "Total"),
    values = c("Predicted"="red", "Actual"="blue", "Total stands"="black")
  ) +
  facet_wrap(~ Address) +
  theme(
    legend.box.background = element_rect(),
    legend.position = "bottom"
  )
base_plot

# Pull the station names as a factor level and create a label df for the annotation
ann_levels <- test %>% group_by(Address) %>% summarise() %>% pull(Address) %>% as.factor()
ann_text <- tibble(Time = "09:00", mean_av_bikes = 33, 
                   Address = factor("Royal Hospital", levels = ann_levels))
test_set_mean_pred <- base_plot + 
  geom_label(data = ann_text, label = err_label, size = 4)
test_set_mean_pred

ggsave("./plots/predictions/test_mean_prediction.png", test_set_mean_pred)


#Gradient Boosting from gbm pacakge
start2.time <- Sys.time()
boost=gbm(av_bikes ~  Weekday + Time.x + prev_bike_num2 +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature,
          data = train,distribution = "gaussian",n.trees = 10000,
          shrinkage = 0.01, interaction.depth = 6)
boost
summary(boost)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
write_rds(boost, "saved_data_frames/gbm_model_with_Weather.rds") # Save GB to avoid running it again
boost
plot(boost)

predict.gbm <- function (object, newdata, n.trees, type = "link", single.tree = FALSE, ...) {
  if (missing(n.trees)) {
    if (object$train.fraction < 1) {
      n.trees <- gbm.perf(object, method = "test", plot.it = FALSE)
    }
    else if (!is.null(object$cv.error)) {
      n.trees <- gbm.perf(object, method = "cv", plot.it = FALSE)
    }
    else {
      n.trees <- length(object$train.error)
    }
    cat(paste("Using", n.trees, "trees...\n"))
    gbm::predict.gbm(object, newdata, n.trees, type, single.tree, ...)
  }
}

test$predGB <- predict.gbm(boost, test)
rmsle(test$predGB, test$av_bikes)
rmse(test$predGB, test$av_bikes)


#Machine learning imlementaitons from caret package

#GBM
fitControl <- trainControl(method = "cv", number = 4)
start2.time <- Sys.time()
newgbm<-train(av_bikes ~  Weekday + Time.x + prev_bike_num +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + holiday, 
              data = train, method = "gbm",
              trControl = fitControl,
              verbose = FALSE, na.action=na.omit)
newgbm
summary(newgbm)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

start2.time <- Sys.time()
newgbm<-train(av_ind ~  Weekday + Time.x + cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + Holiday, 
              data = train, method = "gbm",
              trControl = fitControl,
              verbose = FALSE, na.action=na.omit)
newgbm
summary(newgbm)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

#Predicting the values and calculating the RMSE/RMSLE
test$predGBMCaret <- predict(newgbm, newdata = test, type="raw",na.action =NULL)
rmsle(test$predGBMCaret, test$av_bikes)
rmse(test$predGBMCaret, test$av_bikes)
anyNA(test$predGBMCaret)

table(test$predGBMCaret, test$av_ind)

#Random Forest
fitControl <- trainControl(method = "cv", number = 2)
start2.time <- Sys.time()
newrf<-train(av_bikes ~  Weekday + Time.x + prev_bike_num +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature, data = train, method = "rf",
             trControl = fitControl,
             verbose = FALSE, na.action=na.omit)
newrf
summary(newrf)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

test$predRFCaret <- predict(newrf, newdata = test, type="raw",na.action =NULL)
rmsle(test$predRFCaret, test$av_bikes)
rmse(test$predRFCaret, test$av_bikes)


# GBM - experimental run with a different bike nuum predictor
fitControl <- trainControl(method = "cv", number = 5)
start2.time <- Sys.time()
newgbm<-train(av_bikes ~  Weekday + Time.x + prev_bike_num +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + holiday, 
              data = train, method = "gbm",
              trControl = fitControl,
              verbose = FALSE, na.action=na.omit)
newgbm
summary(newgbm)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

test$predGBMCaret <- predict(newgbm, newdata = test, type="raw",na.action =NULL)

test$predGBMCaret[test$predGBMCaret<0]=0

rmsle(test$predGBMCaret, test$av_bikes)
rmse(test$predGBMCaret, test$av_bikes)

#Elastic Net
library(elasticnet)
start2.time <- Sys.time()
enetModel<-train(av_bikes ~  Weekday + Time.x + prev_bike_num +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + holiday, 
              data = train, method = "enet",
              trControl = fitControl,
              na.action=na.omit)
enetModel
summary(enetModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

test$predENETCaret <- predict(enetModel, newdata = test, type="raw",na.action =NULL)

test$predENETCaret[test$predENETCaret<0]=0

rmsle(test$predENETCaret, test$av_bikes)
rmse(test$predENETCaret, test$av_bikes)

# Linear Regression with Stepwise Selection
start2.time <- Sys.time()
lnrStpModel<-train(av_bikes ~  Weekday + Time.x + prev_bike_num +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + holiday, 
                data = train, method = "leapSeq",
                trControl = fitControl,
                verbose = FALSE, na.action=na.omit)
lnrStpModel
summary(lnrStpModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken


#lnrStpModel<-read_rds("models/lnrStpModel_with_weather_caret.rds")


test$predLNRSTPCaret <- predict(lnrStpModel, newdata = test, type="raw",na.action =NULL)
test$predLNRSTPCaret[test$predLNRSTPCaret<0]=0

rmsle(test$predLNRSTPCaret, test$av_bikes)
rmse(test$predLNRSTPCaret, test$av_bikes)

#Boosted Linear Regression
start2.time <- Sys.time()
bstLmModel<-train(av_bikes ~  Weekday + Time.x + prev_bike_num +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + holiday, 
                   data = train, method = "BstLm",
                   trControl = fitControl,
                   na.action=na.omit)
bstLmModel
summary(bstLmModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken


bstLmModel<-read_rds()
test$predBstLmCaret <- predict(bstLmModel, newdata = test, type="raw",na.action =NULL)

test$predBstLmCaret[test$predBstLmCaret<0]=0

rmsle(test$predBstLmCaret, test$av_bikes)
rmse(test$predBstLmCaret, test$av_bikes)


#KNN
library(kknn)
start2.time <- Sys.time()
knnRegModel<-train(av_bikes ~  Weekday + Time + prev_bike_num +cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + holiday, 
                  data = train, method = "kknn",
                  trControl = fitControl,
                  na.action=na.omit)
knnRegModel
summary(knnRegModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

test$predknnRegCaret <- predict(knnRegModel, newdata = test, type="raw",na.action =na.pass)

test$predknnRegCaret[test$predknnRegCaret<0]=0

rmsle(test$predknnRegCaret, test$av_bikes)
rmse(test$predknnRegCaret, test$av_bikes)


any(is.na(test$predGBMCaret))
which(is.nan(log(test$predGBMCaret+1)))


#SVM - Linear
start2.time <- Sys.time()

ctrl <- trainControl(method = "cv", 
                     summaryFunction = twoClassSummary, 
                     classProbs = TRUE)

svmL<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 + cluster + Latitude + Longitude + season + WindSpeed + AirTemperature, 
              data = train, method = "svmLinear",
              metric = "ROC",
              verbose = FALSE,                    
              trControl = ctrl,
              scale=FALSE,
              na.action=na.omit)
svmL
summary(svmL)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
write_rds(newgbm2, "svmLmodel_with_weather_caret.rds") # Save rf to avoid running it again


#SVM - Radial
start2.time <- Sys.time()

ctrl <- trainControl(method = "cv", 
                     summaryFunction = twoClassSummary, 
                     classProbs = TRUE)

svmR<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 + cluster + Latitude + Longitude + season + WindSpeed + AirTemperature, 
               data = train, method = "svmRadial",
               metric = "ROC",
               verbose = FALSE,                    
               trControl = ctrl,
               scale=FALSE,
               na.action=na.omit)
svmR
summary(svmR)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

write_rds(svmR, "svmRmodel_with_weather_caret.rds") # Save rf to avoid running it again


# GBM- Classification
#start2.time <- Sys.time()
#newgbm3<-train(av_bikes ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 
#               + cluster + Latitude + Longitude + season + WindSpeed + AirTemperature + holiday, 
 #              data = train, method = "gbm",
 #              trControl = fitControl,
 #              verbose = FALSE, na.action=na.omit)
#newgbm3
#summary(newgbm3)
#end2.time <- Sys.time()
#time2.taken <- end2.time - start2.time
#time2.taken
#test$predGBMCaret3 <- predict(newgbm3, newdata = test, type="raw",na.action =NULL)
#rmsle(test$predGBMCaret3, test$av_bikes)
#rmse(test$predGBMCaret3, test$av_bikes)

#sqrt(1/length(test$av_bikes)*sum((log(test$predGBMCaret3 +1)-log(test$av_bikes +1))^2))


#test$predGBMCaret <- predict(newgbm3, newdata = test, type="raw", na.action = na.pass)
#rmsle(test$predGBMCaret, test$av_bikes)
#rmse(test$predGBMCaret, test$av_bikes)
#any(is.na(test$predGBMCaret))
#which(is.nan(log(test$predGBMCaret+1)))

#predGBMCaret <- predict(newgbm3, newdata = test, type="raw", na.action = na.pass)
#length(predGBMCaret)
#length(test$av_bikes)
#rmsle(predGBMCaret, test$av_bikes)
#rmse(predGBMCaret, test$av_bikes)
#any(is.na(predGBMCaret))
#which(is.nan(log(predGBMCaret+1)))

#test$predGBMCaret2 <- predict(newgbm2, newdata = test, type="prob",na.action =NULL)
#pred <- factor(ifelse(test$predGBMCaret2[,'low'] > .6, "low", "high"))

#length(pred)
#length(test$av_ind)


#confusionMatrix(test$av_ind, pred)
#table(test$predGBMCaret2, test$av_ind)
#anyNA(test$predGBMCaret)

## Classification Models

# CLeaning the missing values from previous bike number columns
library(DataCombine)

train2 <- DropNA(train, Var = c("prevWeek_bike_num","prevWeek_bike_num2","prevWeek_bike_num3","prevWeek_bike_num4"))
test2 <- DropNA(test, Var = c("prevWeek_bike_num","prevWeek_bike_num2","prevWeek_bike_num3","prevWeek_bike_num4"))


filter(train2, av_ind=='low')

test2=test2 %>% mutate_if(is.character, as.factor)
train2=train2 %>% mutate_if(is.character, as.factor)
train2$av_ind<-factor(train2$av_ind)
test2$av_ind<-factor(test2$av_ind)

nearZeroVar(train2, saveMetrics = TRUE)

#SVM- Linear
start2.time <- Sys.time()
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svmModel<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 + cluster + Latitude + Longitude + season, 
                     data = train, method = "svmLinear",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     na.action=na.omit)
svmModel
summary(svmModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

test$predSVM <- predict(svmModel, newdata = test, type="prob",na.action =NULL)
pred <- factor(ifelse(test$predSVM[,'low'] > .5, "low", "high"))
confusionMatrix(test$av_ind, pred)

# User defined function for calculating f1 score
f1_score <- function(predicted, expected, positive.class="high") {
  predicted <- factor(as.character(predicted), levels=unique(as.character(expected)))
  expected  <- as.factor(expected)
  cm = as.matrix(table(expected, predicted))
  
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  #Binary F1 or Multi-class macro-averaged F1
  ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
}

#User defined function for calculating MCC score
mcc <- function (actual, predicted, positive="high", negative="low")
{
  # Compute the Matthews correlation coefficient (MCC) score
  # Jeff Hebert 9/1/2016
  # Geoffrey Anderson 10/14/2016 
  # Added zero denominator handling.
  # Avoided overflow error on large-ish products in denominator.
  #
  # actual = vector of true outcomes, 1 = Positive, 0 = Negative
  # predicted = vector of predicted outcomes, 1 = Positive, 0 = Negative
  # function returns MCC
  
  TP <- sum(actual == positive & predicted == positive)
  TN <- sum(actual == negative & predicted == negative)
  FP <- sum(actual == negative & predicted == positive)
  FN <- sum(actual == positive & predicted == negative)
  #TP;TN;FP;FN # for debugging
  sum1 <- TP+FP; sum2 <-TP+FN ; sum3 <-TN+FP ; sum4 <- TN+FN;
  denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
  if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
    denom <- 1
  }
  mcc <- ((TP*TN)-(FP*FN)) / sqrt(denom)
  return(mcc)
}

#KNN
library(kknn)
fitControl <- trainControl(method = "cv", number = 5)

start2.time <- Sys.time()
knnModel<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 +cluster + Latitude + Longitude + season + holiday, 
              data = train2, method = "kknn",
              trControl = fitControl,
              verbose = FALSE, na.action=na.omit)
knnModel
summary(knnModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
test2$knnModel <- predict(knnModel, newdata = test2, type="raw",na.action = na.pass)
#predknnModel <- factor(ifelse(test$knnModel[,'low'] > .5, "low", "high"))
confusionMatrix(test2$av_ind, test2$knnModel)

mcc(test2$av_ind, test2$knnModel)

train2[train2[,'season']=='Summer',]




#LDA
start2.time <- Sys.time()
ldaModel<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 +cluster + Latitude + Longitude + season + holiday, 
                data = train2, method = "lda",
                trControl = fitControl,
                verbose = TRUE, na.action=na.omit)
ldaModel
summary(ldaModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
test2$ldaModel <- predict(ldaModel, newdata = test2, type="raw",na.action = na.pass)
#predldaModel <- factor(ifelse(test2$ldaModel[,'low'] > .4, "low", "high"))
confusionMatrix(test2$av_ind, test2$ldaModel)
mcc(test2$av_ind, test2$ldaModel)

#QDA
start2.time <- Sys.time()
qdaModel<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 +cluster + Latitude + Longitude + season + holiday, 
                data = train2, method = "qda",
                trControl = fitControl,
                verbose = T, na.action=na.pass)
qdaModel
summary(qdaModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
test2$qdaModel <- predict(qdaModel, newdata = test2, type="raw",na.action = na.pass)
#predqdaModel <- factor(ifelse(test2$qdaModel[,'low'] > .5, "low", "high"))

confusionMatrix(test2$av_ind, test2$qdaModel)
mcc(test2$av_ind, test2$qdaModel)

#Logistic Regression
start2.time <- Sys.time()
ctrl <- trainControl(method = "cv", 
                     summaryFunction = twoClassSummary, 
                     classProbs = TRUE)

#logisticModel<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 + cluster + Latitude + Longitude + season + WindSpeed + AirTemperature, 
logisticModel<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 + cluster + Latitude + Longitude + season + holiday, 
                     data = train2, method="glm", family="binomial",
                     na.action=na.omit)
logisticModel
summary(logisticModel)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken

test2$predLogistic <- predict(logisticModel, newdata = test2, type="raw",na.action = na.pass)
#predlogistic <- factor(ifelse(test$predLogistic[,'low'] > .5, "low", "high"))
confusionMatrix(test2$av_ind, test2$predLogistic )

mcc(test2$av_ind, test2$predLogistic)


#GBM - Classification
fitControl <- trainControl(method = "cv", number = 5)
start2.time <- Sys.time()
gbmCategorical<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 +cluster + Latitude + Longitude + season + holiday, 
                      data = train2, method = "gbm",
                      trControl = fitControl,
                      verbose = FALSE, na.action=na.omit)
gbmCategorical
summary(gbmCategorical)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
test2$gbmCategorical <- predict(gbmCategorical, newdata = test2, type="raw",na.action = na.pass)
#predgbmCat <- factor(ifelse(test$gbmCategorical[,'low'] > .5, "low", "high"))
confusionMatrix(test2$av_ind, test2$gbmCategorical)
mcc(test2$av_ind, test2$gbmCategorical)


#Random Frest - Classification
start2.time <- Sys.time()
rfCategorical<-train(av_ind ~  Weekday + Time.x + prevWeek_bike_num + prevWeek_bike_num2 + prevWeek_bike_num3 + prevWeek_bike_num4 +cluster + Latitude + Longitude + season + holiday, 
                      data = train2, method = "rf",
                      trControl = fitControl,
                      verbose = FALSE, na.action=na.omit)
rfCategorical
summary(rfCategorical)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
test2$rfCategorical <- predict(rfCategorical, newdata = test2, type="raw",na.action = na.pass)
#predgbmCat <- factor(ifelse(test$gbmCategorical[,'low'] > .5, "low", "high"))
confusionMatrix(test2$av_ind, test2$rfCategorical)
mcc(test2$av_ind, test2$rfCategorical)

test$predGBRound <- round(test$predLNRSTPCaret)
gb_err <- rmse(test$predGBRound, test$av_bikes)
gb_err


time_breaks <- test %>%
  dplyr::filter(str_detect(Time.x, "\\d{2}:00")) %>%
  group_by(Time.x) %>%
  summarise() %>%
  pull(Time.x)


####################################################################################
# Label for the error
err_label <- paste("Error margin:", round(gb_err, 2))

# plot means per stations
base_plotgb <- test %>%
  group_by(Number, Address.x, Time.x) %>%
  summarise(
    mean_av_bikes = mean(av_bikes),
    mean_predgb = mean(predGBRound),
    tot_stands = max(Bike_stands)
  ) %>%
  ggplot(aes(Time.x, mean_av_bikes, group = 1)) +
  theme_minimal() +
  geom_line(aes(y = tot_stands, colour = "Total stands"), linetype = 6) +
  geom_line(aes(colour = "Actual"), size = 1.1) +
  geom_line(aes(y = mean_predgb, group = 1, colour = "Predicted"), size = 1.1) +
  geom_ribbon(aes(ymin = mean_predgb - gb_err, ymax = mean_predgb + gb_err), fill = "grey30", alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) + 
  scale_x_discrete(
    breaks = time_breaks,
    labels = time_breaks
  ) + 
  ggtitle("Linear Regression with Stepwise Selection  - Predicted vs Actual number") +
  ylab("Mean available bikes") +
  scale_colour_manual(
    breaks = c("Predicted", "Actual", "Total"),
    values = c("Predicted"="red", "Actual"="blue", "Total stands"="black")
  ) +
  facet_wrap(~ Address.x) +
  theme(
    legend.box.background = element_rect(),
    legend.position = "bottom"
  )
base_plotgb

# Pull the station names as a factor level and create a label df for the annotation
ann_levels <- test %>% group_by(Address.x) %>% summarise() %>% pull(Address.x) %>% as.factor()
ann_text <- tibble(Time.x = "09:00", mean_av_bikes = 37, 
                   Address.x = "Merrion Square West"  ) #factor("Merrion Square West", levels = ann_levels))
test_set_mean_predgb <- base_plotgb + 
  geom_label(data = ann_text, label = err_label, size = 4)
test_set_mean_predgb




##### EXPERIMENTAL MODELS ###############
#Gradient Boosting
start2.time <- Sys.time()
boost=gbm(av_bikes ~  Weekday + Time + prevWeek_bike_num +cluster + Latitude + Longitude + season,
          data = train,distribution = "gaussian",n.trees = 10000,
          shrinkage = 0.01, interaction.depth = 6)
boost
summary(boost)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
time2.taken
write_rds(boost, "saved_data_frames/gbm_model-prev_week.rds") # Save GB to avoid running it again
boost


#Gradient Boosting
start2.time <- Sys.time()
boost2=gbm(av_bikes ~  Weekday + Time + prev_bike_num + prevWeek_bike_num +cluster + Latitude + Longitude + season,
          data = train,distribution = "gaussian",n.trees = 10000,
          shrinkage = 0.01, interaction.depth = 6)
boost2
summary(boost2)
end2.time <- Sys.time()
time2.taken <- end2.time - start2.time
print("Boost2")
time2.taken
write_rds(boost2, "C:/MSc Materials/Dissertation/dublin_bikes-master//saved_data_frames/gbm_model-prev_week and prev_bikes.rds") # Save GB to avoid running it again
boost2



predict.gbm <- function (object, newdata, n.trees, type = "link", single.tree = FALSE, ...) {
  if (missing(n.trees)) {
    if (object$train.fraction < 1) {
      n.trees <- gbm.perf(object, method = "test", plot.it = FALSE)
    }
    else if (!is.null(object$cv.error)) {
      n.trees <- gbm.perf(object, method = "cv", plot.it = FALSE)
    }
    else {
      n.trees <- length(object$train.error)
    }
    cat(paste("Using", n.trees, "trees...\n"))
    gbm::predict.gbm(object, newdata, n.trees, type, single.tree, ...)
  }
}

test$predGB <- predict.gbm(boost, test)
rmsle(test$predGB, test$av_bikes)
rmse(test$predGB, test$av_bikes)

table(test2$av_ind)
#######################END ###################################################

############################ Python Data - Plots #############################################

#Reading the output data from python LSTM model
from_python<-read_csv('regOutput-python.csv', col_types = cols('5' = col_character()))

from_python<-from_python %>%
  rename('Number'='0', 'Bike_stands'='1', 'Time'='5', 'av_bikes'='6', 'python_pred'='328')


from_python$predGBRound <- round(from_python$python_pred)
from_python<-from_python %>%
  left_join(geo, by = "Number") 

gb_err <- rmse(from_python$predGBRound, from_python$av_bikes)

#Cleaning the Time column values
for(i in  1:nrow(from_python)) {
  
  if(nchar(from_python[i,"Time"][[1]]) < 5) {
    from_python[i,"Time"] = paste0("0", from_python[i,"Time"])
  }
}

typeof(from_python$Time)

time_breaks2 <- from_python %>%
  filter(str_detect(Time, "\\d{2}:00")) %>%
  group_by(Time) %>%
  summarise() %>%
  pull(Time)


# Label for the error
err_label <- paste("Error margin:", round(gb_err, 2))

# plot means per stations
base_plotgb <- from_python %>%
  group_by(Number, Address, Time) %>%
  summarise(
    mean_av_bikes = mean(av_bikes),
    mean_predgb = mean(predGBRound, na.rm = T),
    tot_stands = max(Bike_stands)
    
  ) %>%
  ggplot(aes(Time, mean_av_bikes, group = 1)) +
  theme_minimal() +
  geom_line(aes(y = tot_stands, colour = "Total stands"), linetype = 6) +
  geom_line(aes(colour = "Actual"), size = 1.1) +
  geom_line(aes(y = mean_predgb, group = 1, colour = "Predicted"), size = 1.1) +
  geom_ribbon(aes(ymin = mean_predgb - gb_err, ymax = mean_predgb + gb_err), fill = "grey30", alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) + 
  scale_x_discrete(
    breaks = time_breaks2,
    labels = time_breaks2
  ) + 
  ggtitle("LSTM - Prediction and Actual number") +
  ylab("Mean available bikes") +
  scale_colour_manual(
    breaks = c("Predicted", "Actual", "Total"),
    values = c("Predicted"="red", "Actual"="blue", "Total stands"="black")
  ) +
  facet_wrap(~ Address) +
  theme(
    legend.box.background = element_rect(),
    legend.position = "bottom"
  )
base_plotgb

# Pull the station names as a factor level and create a label df for the annotation
ann_levels <- from_python %>% group_by(Address) %>% summarise() %>% pull(Address) %>% as.factor()
ann_text <- tibble(Time = "09:00", mean_av_bikes = 33, 
                   Address = factor("Merrion Square West", levels = ann_levels))
test_set_mean_predgb <- base_plotgb + 
  geom_label(data = ann_text, label = err_label, size = 4)
test_set_mean_predgb




#######################################FITTING DISTRIBUTION FUNCTIONS ########################################

# Funtion to estimate the alpha-beta parameters of a beta distribution
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}


#Checking for probability distributions in bike numbers at particular stations

# Group time into 48 factors
dist_data <- all_clust %>%
  mutate(
    t_hour = as.numeric(str_extract(Time, "^\\d{1,2}")),
    t_min = as.numeric(str_extract( str_extract(Time, ":\\d+:"), "\\d+")),
    Time = as.factor(if_else( 
      t_min < 30, 
      if_else(
        t_hour < 10, 
        paste(paste("0", t_hour, sep = ""), "00", sep=":"), 
        paste(t_hour, "00", sep=":")
      ),
      if_else(
        t_hour < 10, 
        paste(paste("0", t_hour, sep = ""), "30", sep=":"), 
        paste(t_hour, "30", sep=":")
      )
    )),
    #Name = as.factor(Name),
    season = as.factor(
      if_else(month(Date) == 11 |month(Date) == 12 | month(Date) == 1, "Winter",
              if_else(month(Date) == 2 |month(Date) == 3 | month(Date) == 4, "Spring",
                      if_else(month(Date) == 5 |month(Date) == 6 |month(Date) == 7, "Summer",
                              "Autumn")))
    ),
    av_bikes = Bike_stands - Available_stands
  ) %>%
  group_by(
    Number, Name.x, Address, Bike_stands, Date, season, Weekday, Time
  ) %>%
  summarise(
    av_bikes = round(mean(av_bikes)),
    cluster = first(cluster)
  ) %>%
  mutate( 
    cluster = as.factor(cluster), 
    prev_bike_num = lag(av_bikes), # number of bikes in previous time period
    prev_bike_num = if_else(is.na(prev_bike_num), av_bikes, prev_bike_num),
    prev_bike_num2 = lag(av_bikes, k=3), # number of bikes in previous time period
    prev_bike_num2 = if_else(is.na(prev_bike_num2), av_bikes, prev_bike_num2)
  ) %>% 
  ungroup()

dim(dist_data)

# Cullen Frey graphs for stations during three timepoints
for(i in c(5,34,19,28,38,15,96,82,30,80)) {
  
  for(j in c('09:00', '13:00', '17:00')) {  
    values<-dist_data[dist_data[,'Number']==i & dist_data[,'Time']==j & dist_data[,'Weekday']=='Sat','av_bikes']
    bike_standsnum<-max(dist_data[dist_data[,'Number']==i,'Bike_stands'])
    values<-values[[1]]
    #descdist((values), boot=50)
    normalized = (values+ 0.001)/(bike_standsnum+ 0.002)
    
    cat('Station: ', i)
    cat(' Time: ', j , '  ')
    
    betaparams<-estBetaParams(mean(normalized), var(normalized))
    print(((1/(1+betaparams$beta/betaparams$alpha))*bike_standsnum))
    print(var(normalized)*bike_standsnum)
    plot(values)
    abline(h=((1/(1+betaparams$beta/betaparams$alpha))*bike_standsnum))
  }
}

#HUESTON CENTRAL - Monday
#----------------

# 9:00 - > 0.11
# 13:00 -> 0.07
# 17:00 -> 0.6

#CHARLEMONT - Monday
#----------------

# 9:00 - > 0.19
# 13:00 -> 0.24
# 17:00 -> 0.5


#Kolmogorov-Smirnov Test
ks.test(hueston, rnorm(1000))
typeof(hueston)

#Shapiro-Wilk Test for Normality
shapiro.test(hueston)

skewness(hueston)
hist(hueston)
hist(log(hueston))
estBetaParams(mean(hueston), var(hueston))
descdist(normalized)
fit.beta <- fitdist((normalized), "beta")














################################### NOT INCLUDED IN THE REPORT ##########################################

library('forecast')
library('tseries')
library(zoo)
library(forecast)
library(chron)
library(plyr)
library(caret)
library(foreach)
library(doMC)
library(parallel)
library(dplyr)
library(ggplot2)


#Running ARIMA for only one station

bensonSt <- rf_df2[rf_df2[,"Number"]==90 & rf_df2[,"Date"] != "2016-09-12" ,]

nrow(bensonSt)
nrow(bensonSt[bensonSt[,"Date"]=="2016-09-12",])

btrain=bensonSt[bensonSt$Date<="2017-07-30",]
btest=bensonSt[bensonSt$Date>"2017-07-30",]

constant_columns <- apply(bensonSt, 2, is.constant)
if (any(constant_columns)) { # Remove first one
  print(constant_columns[2])
  #xregg <- xregg[, -which(constant_columns)[1]]
}

trainC=ts(btrain,frequency = 48)

btest2 = btest[,-c(1:5,9:10,13:14)]
testC=ts(btest2,frequency = 48)

arimaCols <- c("Weekday" , "Time" ,"prev_bike_num" , "prevWeek_bike_num", "season")

#arimaModA=suppressWarnings(auto.arima(trainC[,"av_bikes"],xreg=trainC[,arimaCols],stepwise=F, approximation=T))

arimaModA=(auto.arima(trainC[,"av_bikes"],xreg=trainC[,arimaCols],seasonal=FALSE, stepwise=F, approximation=T))
arimaModf=forecast(arimaModA, xreg=testC)
library(readr)
write_rds(arimaModA, "C:/MSc Materials/Dissertation/dublin_bikes-master/saved_data_frames/arima-bensonst.rds") 

btestMerged <- merge(btest, arimaModf$mean, by="row.names")

base_plotgbArima <- btestMerged %>%
  group_by(Number, Address, Time) %>%
  summarise(
    mean_av_bikes = mean(av_bikes),
    mean_predarima = mean(x),
    tot_stands = max(Bike_stands)
  ) %>%
  ggplot(aes(Time, mean_av_bikes, group = 1)) +
  theme_minimal() +
  geom_line(aes(y = tot_stands, colour = "Total stands"), linetype = 6) +
  geom_line(aes(colour = "Actual"), size = 1.1) +
  geom_line(aes(y = mean_predarima, group = 1, colour = "Predicted"), size = 1.1)




wtSt <- rf_df2[rf_df2[,"Number"]==56 & rf_df2[,"Date"] != "2016-09-12" ,]

nrow(wtSt)
nrow(wtSt[wtSt[,"Date"]=="2016-09-12",])

wtrain=wtSt[wtSt$Date<="2017-07-30",]
wtest=wtSt[wtSt$Date>"2017-07-30",]

wconstant_columns <- apply(wtSt, 2, is.constant)
if (any(wconstant_columns)) { 
  print(constant_columns[2])
  #xregg <- xregg[, -which(constant_columns)[1]]
}

wtrainC=ts(wtrain,frequency = 48)

wtest2 = wtest[,-c(1:5,9:10,13:14)]
wtestC=ts(wtest2,frequency = 48)

arimaCols <- c("Weekday" , "Time" ,"prev_bike_num" , "prevWeek_bike_num", "season")

#arimaModA=suppressWarnings(auto.arima(trainC[,"av_bikes"],xreg=trainC[,arimaCols],stepwise=F, approximation=T))

warimaModA=(auto.arima(wtrainC[,"av_bikes"],xreg=wtrainC[,arimaCols],seasonal=FALSE, stepwise=F, approximation=T))
warimaModf=forecast(warimaModA, xreg=wtestC)
library(readr)
write_rds(warimaModA, "C:/MSc Materials/Dissertation/dublin_bikes-master/saved_data_frames/arima-mountst.rds") 

wtestMerged <- merge(wtest, warimaModf$mean, by="row.names")

base_plotgbArimamst <- wtestMerged %>%
  group_by(Number, Address, Time) %>%
  summarise(
    mean_av_bikes = mean(av_bikes),
    mean_predarima = mean(x),
    tot_stands = max(Bike_stands)
  ) %>%
  ggplot(aes(Time, mean_av_bikes, group = 1)) +
  theme_minimal() +
  geom_line(aes(y = tot_stands, colour = "Total stands"), linetype = 6) +
  geom_line(aes(colour = "Actual"), size = 1.1) +
  geom_line(aes(y = mean_predarima, group = 1, colour = "Predicted"), size = 1.1)



count_ts = ts(test[, c('av_bikes')])

test$clean_cnt = tsclean(count_ts)

test2 <- arrange(test, Date, Time)

fit2arima <- auto.arima(test2$av_bikes, seasonal=FALSE)

fcast <- forecast(fit2arima, h=30)
plot(fcast)

acf(ts(diff(log10(test2$av_bikes))),main='')
pacf(ts(diff(log10(test2))),main='')


head(df2)



##################### PARETO DISTRIBUTION ##################
library(PtProcess)
library(actuar)
fit.pareto <- fitdist(normalized, "pareto", start=list(shape=2, scale=600))

plot(fit.pareto)

# distribution, cdf, quantile and random functions for Pareto distributions
dpareto <- function(x, xm, alpha) ifelse(x > xm , alpha*xm**alpha/(x**(alpha+1)), 0)
ppareto <- function(q, xm, alpha) ifelse(q > xm , 1 - (xm/q)**alpha, 0 )
qpareto <- function(p, xm, alpha) ifelse(p < 0 | p > 1, NaN, xm*(1-p)**(-1/alpha))
rpareto <- function(n, xm, alpha) qpareto(runif(n), xm, alpha)


pareto.mle <- function(x)
{
  xm <- min(x)
  alpha <- length(x)/(sum(log(x))-length(x)*log(xm))
  return( list(xm = xm, alpha = alpha))
}


pareto.test <- function(x, B = 1e3)
{
  a <- pareto.mle(x)
  
  # KS statistic
  D <- ks.test(x, function(q) ppareto(q, a$xm, a$alpha))$statistic
  
  # estimating p value with parametric bootstrap
  B <- 1e5
  n <- length(x)
  emp.D <- numeric(B)
  for(b in 1:B)
  {
    xx <- rpareto(n, a$xm, a$alpha);
    aa <- pareto.mle(xx)
    emp.D[b] <- ks.test(xx, function(q) ppareto(q, aa$xm, aa$alpha))$statistic
  }
  
  return(list(xm = a$xm, alpha = a$alpha, D = D, p = sum(emp.D > D)/B))
}

pareto.test(hueston)
