# Background:
## Hourly rent information for days on and before 19th of each month in 2 years
# Data Fields:
## Date Time: POSIXlt
## Season: 1 = spring, 2 = summer, 3 = fall, 4 = winter
## Holiday: 0 or 1, whether the day is considered a holiday
## WorkingDay: 0 or 1, whether the day is neither a weekend nor holiday
## Weather: 1: Clear, Few clouds, Partly cloudy, Partly cloudy, 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist, 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds, 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
## Temp: temperature in Celsius
## ATemp: "feels like" temperature in Celsius
## Humidity: relative humidity
## WindSpeed: wind speed
## Casual: number of non-registered user rentals initiated
## Registered: number of registered user rentals initiated
## Count: number of total rentals

# Data Processing:
setwd("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/Bikeshare")
library(caret)
library(ggplot2)
library(plyr)
## training set
training <- read.csv("train.csv", header = T, stringsAsFactors = F)
training$DateTime <- strptime(training$datetime, format = "%Y-%m-%d %H:%M:%S")
training$datetime <- NULL
training <- training[order(training$DateTime), ]
training$Hour <- factor(training$DateTime$hour)
training$Weekday <- factor(weekdays(training$DateTime))
training$season <- factor(training$season)
training$holiday <- factor(training$holiday)
training$workingday <- factor(training$workingday)
training$weather <- factor(training$weather)
training$Date <- as.Date(training$DateTime)
training$ID <- 1:length(training$Date)
training$Ds <- as.numeric(as.Date(training$DateTime) - as.Date(training$DateTime[1])) + training$DateTime$hour/24
training$Day <- factor(training$DateTime$mday)
training <- data.frame(training[, 16], training[, 12], training[, 15], training[, 18], training[, 17], training[, 14], training[, 13], training[, 1:8], training[, 11])
names(training) <- c("ID", "DateTime", "Date", "Day", "Ds", "Weekday", "Hour", "Season", "Holiday", "Workday", "Weather", "Temp", "ATemp", "Humidity", "WindSpeed", "Count")
training <- training[order(training$DateTime), ]
training$DateTime <- as.POSIXlt(training$DateTime)
## Log Transformation
training$CountLog <- log(training$Count)
training$DsLog <- log(training$Ds + 0.001)
training$TempLog <- log(training$Temp)
training$ATempLog <- log(training$ATemp)
training$HumidityLog <- log(training$Humidity + 0.001)
training$WindSpeedLog <- log(training$WindSpeed + 0.001)

## Split into Train and Test data sets, days between 1st and 15th will be in Train, and days between 16th and 19th will be in Test
Train <- training[training$Day %in% as.character(1:15), ]
Test <- training[training$Day %in% as.character(16:19), ]
Test$ActualCount <- Test$Count
Test$ActualCountLog <- Test$CountLog
Test$Count <- rep(0, nrow(Test))
Test$CountLog <- rep(0, nrow(Test))
Test$HLag1CountLog <- rep(0, nrow(Test))
Test$HLag1Delay <- rep(0, nrow(Test))
Test$HLag1Int <- rep(0, nrow(Test))
Test$HLag2CountLog <- rep(0, nrow(Test))
Test$HLag2Delay <- rep(0, nrow(Test))
Test$HLag2Int <- rep(0, nrow(Test))
Test$HLag3CountLog <- rep(0, nrow(Test))
Test$HLag3Delay <- rep(0, nrow(Test))
Test$HLag3Int <- rep(0, nrow(Test))
Test$HLag4CountLog <- rep(0, nrow(Test))
Test$HLag4Delay <- rep(0, nrow(Test))
Test$HLag4Int <- rep(0, nrow(Test))
Test$HLag5CountLog <- rep(0, nrow(Test))
Test$HLag5Delay <- rep(0, nrow(Test))
Test$HLag5Int <- rep(0, nrow(Test))


# Evaluation Formula (Root Mean Squared Logarithmic Error):
RMSLE <- function(Predicted, Reference) {
    n <- length(Predicted)
    sqrt(sum((log(Predicted + 1) - log(Reference + 1))^2)/n)
}

# Exploratory Analysis
## Plot Autocorrelation Function (ACF), and we could see:
## There is alternative dependence (seasonality) with about 24 periods interval (presumably means 24 hours).
acf(Train$Count, lag.max = 100)

## Plot ACF for 24 hours:
## Now we could see there is alternative dependence (seasonality) for about 7 periods (presumably means 7 days in a week).
acf(Train$Count[Train$Hour == 8], lag.max= 50)

## Plot ACF for both 24 hour and 7 days in a week, and here we check few different day and hour:
## 1. There is almost no alternative dependence (seasonality)
## 2. There are about 5 lags that are above significance boundaries
acf(Train$Count[Train$Hour == 16 & Train$Weekday == "Tuesday"])
acf(Train$Count[Train$Hour == 8 & Train$Weekday == "Sunday"])

## Plot ACF for weekly seasonality, and we could see there is 4-6 week lags
Temp <- ddply(Train, .(Date, Weekday), summarize, Count = sum(Count))
acf(Temp[Temp$Weekday == "Monday", ]$Count)
acf(Temp[Temp$Weekday == "Tuesday", ]$Count)
acf(Temp[Temp$Weekday == "Wednesday", ]$Count)
acf(Temp[Temp$Weekday == "Thursday", ]$Count)
acf(Temp[Temp$Weekday == "Friday", ]$Count)
acf(Temp[Temp$Weekday == "Saturday", ]$Count)
acf(Temp[Temp$Weekday == "Sunday", ]$Count)


## Plot the cycle of 24 hours
### There is significant difference in each hour
ggplot(Train, aes(x = Hour, y = Count)) + geom_boxplot(aes(fill = Hour)) + scale_fill_hue(guide = F)
### Plot the Jan 2011 rent, and we could see there is claer seasonality
ggplot(Train[Train$DateTime$year == 111 & Train$DateTime$mon == 0, ], aes(x = DateTime, y = Count)) + geom_point() + geom_line()
### Plot the first 5 days of Jan 2011 to have a closer look, and we could see for each day, there are 2 peaks (around 8AM and 6PM), and there is also one trough at the end of each day.
ggplot(Train[Train$DateTime$year == 111 & Train$DateTime$mon == 0 & Train$DateTime$yday <= 5, ], aes(x = DateTime, y = Count)) + geom_point() + geom_line()
### Plot the count for each of the day in a month, and we do not see a pattern, so we do not need to consider monthly seasonality.
ggplot(Train, aes(x = factor(Day), y = Count)) + geom_boxplot(aes(fill = factor(Day))) + scale_fill_hue(guide = F)

## Plot the weekly seasonality
### Plot the rent of 8AM for each different weekday. Apparently, there is significant different between weekdays and weekends.
ggplot(Train[Train$Hour == 8, ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Weekday))
### Workday plot also shows this pattern.
ggplot(Train[Train$Hour == 8, ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Workday))


## Plot Month day, so significant difference
ggplot(Train[Train$Day %in% c("1", "5", "10", "15") & Train$Date < as.Date("2012-01-01"), ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Day))



## Add 5 lags in hour and do log transformation
Train$HLag1CountLog <- log(c(NA, Train$Count[1:(length(Train$Count) - 1)]))
Train$HLag1Delay <- c(NA, Train$Ds[2:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 1)])
Train$HLag1Delay <- ifelse(Train$HLag1Delay > 1, 0, Train$HLag1Delay * 24)
Train$HLag1CountLog <- ifelse(Train$HLag1Delay == 0, 0, Train$HLag1CountLog)
Train$HLag1Int <- ifelse(Train$HLag1Delay == 0, 0, Train$HLag1CountLog / Train$HLag1Delay)
Train$HLag2CountLog <- log(c(NA, NA, Train$Count[1:(length(Train$Count) - 2)]))
Train$HLag2Delay <- c(NA, NA, Train$Ds[3:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 2)])
Train$HLag2Delay <- ifelse(Train$HLag2Delay > 1, 0, Train$HLag2Delay * 24)
Train$HLag2CountLog <- ifelse(Train$HLag2Delay == 0, 0, Train$HLag2CountLog)
Train$HLag2Int <- ifelse(Train$HLag2Delay == 0, 0, Train$HLag2CountLog / Train$HLag2Delay)
Train$HLag3CountLog <- log(c(NA, NA, NA, Train$Count[1:(length(Train$Count) - 3)]))
Train$HLag3Delay <- c(NA, NA, NA, Train$Ds[4:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 3)])
Train$HLag3Delay <- ifelse(Train$HLag3Delay > 1, 0, Train$HLag3Delay * 24)
Train$HLag3CountLog <- ifelse(Train$HLag3Delay == 0, 0, Train$HLag3CountLog)
Train$HLag3Int <- ifelse(Train$HLag3Delay == 0, 0, Train$HLag3CountLog / Train$HLag3Delay)
Train$HLag4CountLog <- log(c(NA, NA, NA, NA, Train$Count[1:(length(Train$Count) - 4)]))
Train$HLag4Delay <- c(NA, NA, NA, NA, Train$Ds[5:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 4)])
Train$HLag4Delay <- ifelse(Train$HLag4Delay > 1, 0, Train$HLag4Delay * 24)
Train$HLag4CountLog <- ifelse(Train$HLag4Delay == 0, 0, Train$HLag4CountLog)
Train$HLag4Int <- ifelse(Train$HLag4Delay == 0, 0, Train$HLag4CountLog / Train$HLag4Delay)
Train$HLag5CountLog <- log(c(NA, NA, NA, NA, NA, Train$Count[1:(length(Train$Count) - 5)]))
Train$HLag5Delay <- c(NA, NA, NA, NA, NA, Train$Ds[6:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 5)])
Train$HLag5Delay <- ifelse(Train$HLag5Delay > 1, 0, Train$HLag5Delay * 24)
Train$HLag5CountLog <- ifelse(Train$HLag5Delay == 0, 0, Train$HLag5CountLog)
Train$HLag5Int <- ifelse(Train$HLag5Delay == 0, 0, Train$HLag5CountLog / Train$HLag5Delay)


## Number of non-NA observations: 8595/8600.
sum(complete.cases(Train$HLag1Int, Train$HLag2Int, Train$HLag3Int, Train$HLag4Int, Train$HLag5Int))


# Data quality check
## Holiday and HLagDelay are near zero variance variables, so the quality is decent
nearZeroVar(Train[, c(8:37)], saveMetrics = T)
## Temp is highly correlated with ATemp.
round(cor(Train[, c(12:16)]), 2)
findCorrelation(cor(Train[, c(12:16)]), cutoff = 0.75, verbose = T)
## No linear combination is found
findLinearCombos(Train[, c(12:37)][complete.cases(Train[, c(12:37)]), ])


# Build models
Formula <- formula(CountLog ~ DsLog + Hour + Weekday + Season + Holiday + Workday + Weather + TempLog + ATempLog + HumidityLog + WindSpeedLog + HLag1CountLog + HLag1Delay + HLag1Int + HLag2CountLog + HLag2Delay + HLag2Int + HLag3CountLog + HLag3Delay + HLag3Int + HLag4CountLog + HLag4Delay + HLag4Int + HLag5CountLog + HLag5Delay + HLag5Int)
TrainH5 <- Train[complete.cases(Train[, c("HLag1Int", "HLag2Int", "HLag3Int", "HLag4Int", "HLag5Int")]), ]

## User-Defined Prediction Function
PREDICT <- function(Model) {
    FullTraining <- rbind(data.frame(Train, Set = rep("Train", nrow(Train))), data.frame(Test[, c(1:22, 25:39)], Set = rep("Test", nrow(Test))))
    FullTraining <- FullTraining[order(FullTraining$ID), ]
    for (i in 6:nrow(FullTraining)) {
        if (FullTraining[i, "Set"] == "Test") {
            ## fill lag 1
            if((FullTraining[i, "Ds"] - FullTraining[i - 1, "Ds"]) < 1) {
                FullTraining[i, "HLag1CountLog"] <- FullTraining[i - 1, "CountLog"]
                FullTraining[i, "HLag1Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 1, "Ds"]) * 24
                FullTraining[i, "HLag1Int"] <- FullTraining[i, "HLag1CountLog"] / FullTraining[i, "HLag1Delay"]
            } else {
                FullTraining[i, "HLag1CountLog"] <- 0
                FullTraining[i, "HLag1Delay"] <- 0
                FullTraining[i, "HLag1Int"] <- 0
            }
            ## fill lag 2
            if((FullTraining[i, "Ds"] - FullTraining[i - 2, "Ds"]) < 1) {
                FullTraining[i, "HLag2CountLog"] <- FullTraining[i - 2, "CountLog"]
                FullTraining[i, "HLag2Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 2, "Ds"]) * 24
                FullTraining[i, "HLag2Int"] <- FullTraining[i, "HLag2CountLog"] / FullTraining[i, "HLag2Delay"]
            } else {
                FullTraining[i, "HLag2CountLog"] <- 0
                FullTraining[i, "HLag2Delay"] <- 0
                FullTraining[i, "HLag2Int"] <- 0
            }
            ## fill lag 3
            if((FullTraining[i, "Ds"] - FullTraining[i - 3, "Ds"]) < 1) {
                FullTraining[i, "HLag3CountLog"] <- FullTraining[i - 3, "CountLog"]
                FullTraining[i, "HLag3Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 3, "Ds"]) * 24
                FullTraining[i, "HLag3Int"] <- FullTraining[i, "HLag3CountLog"] / FullTraining[i, "HLag3Delay"]
            } else {
                FullTraining[i, "HLag3CountLog"] <- 0
                FullTraining[i, "HLag3Delay"] <- 0
                FullTraining[i, "HLag3Int"] <- 0
            }
            ## fill lag 4
            if((FullTraining[i, "Ds"] - FullTraining[i - 4, "Ds"]) < 1) {
                FullTraining[i, "HLag4CountLog"] <- FullTraining[i - 3, "CountLog"]
                FullTraining[i, "HLag4Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 4, "Ds"]) * 24
                FullTraining[i, "HLag4Int"] <- FullTraining[i, "HLag4CountLog"] / FullTraining[i, "HLag4Delay"]
            } else {
                FullTraining[i, "HLag4CountLog"] <- 0
                FullTraining[i, "HLag4Delay"] <- 0
                FullTraining[i, "HLag4Int"] <- 0
            }
            ## fill lag 5
            if((FullTraining[i, "Ds"] - FullTraining[i - 5, "Ds"]) < 1) {
                FullTraining[i, "HLag5CountLog"] <- FullTraining[i - 3, "CountLog"]
                FullTraining[i, "HLag5Delay"] <- (FullTraining[i, "Ds"] - FullTraining[i - 5, "Ds"]) * 24
                FullTraining[i, "HLag5Int"] <- FullTraining[i, "HLag5CountLog"] / FullTraining[i, "HLag5Delay"]
            } else {
                FullTraining[i, "HLag5CountLog"] <- 0
                FullTraining[i, "HLag5Delay"] <- 0
                FullTraining[i, "HLag5Int"] <- 0
            }
            FullTraining[i, "CountLog"] <- predict(Model, FullTraining[i, ])
            FullTraining[i, "Count"] <- exp(FullTraining[i, "CountLog"])
        }
    }
    return (FullTraining[FullTraining$Set == "Test", "Count"])
}


## Full Model, Adj R-square: 92.9%, IS RMSLE: 0.358, OOS RMSLE: 0.6
fullModel <- lm(Formula, data = TrainH5)
### In-Sample Prediction and RMSLE
fullPredIS <- predict(fullModel, newdata = TrainH5)
(fullRMSLEIS <- RMSLE(exp(fullPredIS), TrainH5[, "Count"]))
ggplot(NULL, aes(y = fullPredIS, x = TrainH5[, "CountLog"])) + geom_point()
### Out-of-Sample Prediction and RMSLE
fullPredOOS <- PREDICT(fullModel)
(fullRMSLEOOS <- RMSLE(fullPredOOS, Test$ActualCount))
ggplot(NULL, aes(y = log(fullPredOOS), x = Test[, "ActualCountLog"])) + geom_point()


## Stepwise, IS RMSLE: 0.358, OOS RMSLE: 0.6, exactly the same as full model (means the majority of the variables are significant, and step model is very similar to full model)
nullModel <- lm(CountLog ~ 1, data = TrainH5)
stepModel <- step(nullModel, scope = Formula, dir = "both")
### In-Sample Prediction and RMSLE
stepPredIS <- predict(stepModel, newdata = TrainH5)
(stepRMSLEIS <- RMSLE(exp(stepPredIS), TrainH5[, "Count"]))
ggplot(NULL, aes(y = stepPredIS, x = TrainH5[, "CountLog"])) + geom_point()
### Out-of-Sample Prediction and RMSLE
stepPredOOS <- PREDICT(stepModel)
(stepRMSLEOOS <- RMSLE(stepPredOOS, Test$ActualCount))
ggplot(NULL, aes(y = log(stepPredOOS), x = Test[, "ActualCountLog"])) + geom_point()






################### Do not run codes under this line ######################



## Lasso model. RMSLE is 0.68, better than gaussian model
trainingTm3Lasso <- model.matrix(Formula, data = trainingTm3)[, -1]
lassoModel <- glmnet(trainingTm3Lasso, trainingTm3[, "Count"])
lassoPred <- predict(lassoModel, newx = trainingTm3Lasso)
lassoPred <- ifelse(lassoPred < 0, 0, round(lassoPred))
lassoRMSLE <- apply(lassoPred, 2, RMSLE, Reference = trainingTm3[, "Count"])
min(lassoRMSLE)
lassoModel$lambda[which.min(lassoRMSLE)]    # 44th lambda
### CV Lasso
lassoCVModel <- cv.glmnet(trainingTm3Lasso, trainingTm3[, "Count"], nfolds = 10)
lassoCVPred <- predict(lassoCVModel, newx = trainingTm3Lasso)
lassoCVPred <- as.vector(lassoCVPred)
lassoCVPred <- ifelse(lassoCVPred < 0, 0, round(lassoCVPred))
lassoCVRMSLE <- RMSLE(lassoCVPred, trainingTm3[, "Count"])



