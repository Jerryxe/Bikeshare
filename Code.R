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
library(alr4)
library(glmnet)
source("Func.R")
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
training$CountLog <- log(training$Count + 1)
training$DsLog <- log(training$Ds + 1)
training$TempLog <- log(training$Temp + 1)
training$ATempLog <- log(training$ATemp + 1)
training$HumidityLog <- log(training$Humidity + 1)
training$WindSpeedLog <- log(training$WindSpeed + 1)
## BoxCox Power Transformation
PT <- powerTransform(cbind(Count + 1, Ds + 1, Temp + 1, ATemp + 1, Humidity + 1, WindSpeed + 1) ~ 1, data = training)
training$CountPT <- (training$Count + 1)^PT$roundlam[1]
training$DsPT <- (training$Count + 1)^PT$roundlam[2]
training$TempPT <- (training$Count + 1)^PT$roundlam[3]
training$WindSpeedPT <- (training$Count + 1)^PT$roundlam[6]

ggplot(training, aes(x = Temp)) + geom_histogram()

## Split into Train and Test data sets, days between 1st and 15th will be in Train, and days between 16th and 19th will be in Test
Train <- training[training$Day %in% as.character(1:15), ]
Test <- training[training$Day %in% as.character(16:19), ]
Test$ActualCount <- Test$Count
Test$ActualCountLog <- Test$CountLog
Test$ActualCountPT <- Test$CountPT
Test$Count <- rep(0, nrow(Test))
Test$CountLog <- rep(0, nrow(Test))
Test$CountPT <- rep(0, nrow(Test))
Test$HLag1CountLog <- rep(0, nrow(Test))
Test$HLag1CountPT <- rep(0, nrow(Test))
Test$HLag1Delay <- rep(0, nrow(Test))
Test$HLag1LogInt <- rep(0, nrow(Test))
Test$HLag1PTInt <- rep(0, nrow(Test))
Test$HLag2CountLog <- rep(0, nrow(Test))
Test$HLag2CountPT <- rep(0, nrow(Test))
Test$HLag2Delay <- rep(0, nrow(Test))
Test$HLag2LogInt <- rep(0, nrow(Test))
Test$HLag2PTInt <- rep(0, nrow(Test))
Test$HLag3CountLog <- rep(0, nrow(Test))
Test$HLag3CountPT <- rep(0, nrow(Test))
Test$HLag3Delay <- rep(0, nrow(Test))
Test$HLag3LogInt <- rep(0, nrow(Test))
Test$HLag3PTInt <- rep(0, nrow(Test))
Test$HLag4CountLog <- rep(0, nrow(Test))
Test$HLag4CountPT <- rep(0, nrow(Test))
Test$HLag4Delay <- rep(0, nrow(Test))
Test$HLag4LogInt <- rep(0, nrow(Test))
Test$HLag4PTInt <- rep(0, nrow(Test))
Test$HLag5CountLog <- rep(0, nrow(Test))
Test$HLag5CountPT <- rep(0, nrow(Test))
Test$HLag5Delay <- rep(0, nrow(Test))
Test$HLag5LogInt <- rep(0, nrow(Test))
Test$HLag5PTInt <- rep(0, nrow(Test))

# Exploratory Analysis
## Plot Autocorrelation Function (ACF), and we could see:
## There is alternative dependence (seasonality) with about 24 periods interval (presumably means 24 hours).
plot(acf(Train$Count, lag.max = 100), main = "Auto Correlation Function", xlab = "Hourly Lags", ylab = "Correlation")

## Plot ACF for 24 hours:
## Now we could see there is alternative dependence (seasonality) for about 7 periods (presumably means 7 days in a week).
acf(Train$Count[Train$Hour == 8], lag.max= 50, main = "Auto Correlation Function", xlab = "Daily Lags", ylab = "Correlation")

## Plot ACF for both 24 hour and 7 days in a week, and here we check few different day and hour:
## 1. There is almost no alternative dependence (seasonality)
## 2. There are about 5 lags that are above significance boundaries
acf(Train$Count[Train$Hour == 16 & Train$Weekday == "Tuesday"], main = "Auto Correlation Function", xlab = "Weekly Lags", ylab = "Correlation")
acf(Train$Count[Train$Hour == 8 & Train$Weekday == "Sunday"], main = "Auto Correlation Function", xlab = "Weekly Lags", ylab = "Correlation")


## Plot ACF for weekly seasonality, and we could see there is 4-6 week lags
Temp <- ddply(Train, .(Date, Weekday), summarize, Count = sum(Count))
acf(Temp[Temp$Weekday == "Tuesday", ]$Count, main = "Auto Correlation Function", xlab = "Weekly Lags", ylab = "Correlation")
acf(Temp[Temp$Weekday == "Sunday", ]$Count, main = "Auto Correlation Function", xlab = "Weekly Lags", ylab = "Correlation")


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
ggplot(Train[Train$Hour == 8, ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Weekday)) + scale_color_hue(guide = F)
### Workday plot also shows this pattern.
ggplot(Train[Train$Hour == 8, ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Workday)) + scale_color_hue(guide = F)


## Plot Month day, so significant difference
ggplot(Train[Train$Day %in% c("1", "5", "10", "15") & Train$Date < as.Date("2012-01-01"), ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Day))



## Add 5 lags in hour and do log transformation
Train$HLag1CountLog <- c(NA, Train$CountLog[1:(length(Train$CountLog) - 1)])
Train$HLag1CountPT <- c(NA, Train$CountPT[1:(length(Train$CountPT) - 1)])
Train$HLag1Delay <- c(NA, Train$Ds[2:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 1)])
Train$HLag1Delay <- ifelse(Train$HLag1Delay > 1, 0, Train$HLag1Delay * 24)
Train$HLag1CountLog <- ifelse(Train$HLag1Delay == 0, 0, Train$HLag1CountLog)
Train$HLag1CountPT <- ifelse(Train$HLag1Delay == 0, 0, Train$HLag1CountPT)
Train$HLag1LogInt <- ifelse(Train$HLag1Delay == 0, 0, Train$HLag1CountLog / Train$HLag1Delay)
Train$HLag1PTInt <- ifelse(Train$HLag1Delay == 0, 0, Train$HLag1CountPT / Train$HLag1Delay)

Train$HLag2CountLog <- c(NA, NA, Train$CountLog[1:(length(Train$CountLog) - 2)])
Train$HLag2CountPT <- c(NA, NA, Train$CountPT[1:(length(Train$CountPT) - 2)])
Train$HLag2Delay <- c(NA, NA, Train$Ds[3:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 2)])
Train$HLag2Delay <- ifelse(Train$HLag2Delay > 1, 0, Train$HLag2Delay * 24)
Train$HLag2CountLog <- ifelse(Train$HLag2Delay == 0, 0, Train$HLag2CountLog)
Train$HLag2CountPT <- ifelse(Train$HLag2Delay == 0, 0, Train$HLag2CountPT)
Train$HLag2LogInt <- ifelse(Train$HLag2Delay == 0, 0, Train$HLag2CountLog / Train$HLag2Delay)
Train$HLag2PTInt <- ifelse(Train$HLag2Delay == 0, 0, Train$HLag2CountPT / Train$HLag2Delay)

Train$HLag3CountLog <- c(NA, NA, NA, Train$CountLog[1:(length(Train$CountLog) - 3)])
Train$HLag3CountPT <- c(NA, NA, NA, Train$CountPT[1:(length(Train$CountPT) - 3)])
Train$HLag3Delay <- c(NA, NA, NA, Train$Ds[4:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 3)])
Train$HLag3Delay <- ifelse(Train$HLag3Delay > 1, 0, Train$HLag3Delay * 24)
Train$HLag3CountLog <- ifelse(Train$HLag3Delay == 0, 0, Train$HLag3CountLog)
Train$HLag3CountPT <- ifelse(Train$HLag3Delay == 0, 0, Train$HLag3CountPT)
Train$HLag3LogInt <- ifelse(Train$HLag3Delay == 0, 0, Train$HLag3CountLog / Train$HLag3Delay)
Train$HLag3PTInt <- ifelse(Train$HLag3Delay == 0, 0, Train$HLag3CountPT / Train$HLag3Delay)

Train$HLag4CountLog <- c(NA, NA, NA, NA, Train$CountLog[1:(length(Train$CountLog) - 4)])
Train$HLag4CountPT <- c(NA, NA, NA, NA, Train$CountPT[1:(length(Train$CountPT) - 4)])
Train$HLag4Delay <- c(NA, NA, NA, NA, Train$Ds[5:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 4)])
Train$HLag4Delay <- ifelse(Train$HLag4Delay > 1, 0, Train$HLag4Delay * 24)
Train$HLag4CountLog <- ifelse(Train$HLag4Delay == 0, 0, Train$HLag4CountLog)
Train$HLag4CountPT <- ifelse(Train$HLag4Delay == 0, 0, Train$HLag4CountPT)
Train$HLag4LogInt <- ifelse(Train$HLag4Delay == 0, 0, Train$HLag4CountLog / Train$HLag4Delay)
Train$HLag4PTInt <- ifelse(Train$HLag4Delay == 0, 0, Train$HLag4CountPT / Train$HLag4Delay)

Train$HLag5CountLog <- c(NA, NA, NA, NA, NA, Train$CountLog[1:(length(Train$CountLog) - 5)])
Train$HLag5CountPT <- c(NA, NA, NA, NA, NA, Train$CountPT[1:(length(Train$CountPT) - 5)])
Train$HLag5Delay <- c(NA, NA, NA, NA, NA, Train$Ds[6:length(Train$Ds)] - Train$Ds[1:(length(Train$Ds) - 5)])
Train$HLag5Delay <- ifelse(Train$HLag5Delay > 1, 0, Train$HLag5Delay * 24)
Train$HLag5CountLog <- ifelse(Train$HLag5Delay == 0, 0, Train$HLag5CountLog)
Train$HLag5CountPT <- ifelse(Train$HLag5Delay == 0, 0, Train$HLag5CountPT)
Train$HLag5LogInt <- ifelse(Train$HLag5Delay == 0, 0, Train$HLag5CountLog / Train$HLag5Delay)
Train$HLag5PTInt <- ifelse(Train$HLag5Delay == 0, 0, Train$HLag5CountPT / Train$HLag5Delay)
## Number of non-NA observations: 8595/8600.
sum(complete.cases(Train$HLag1LogInt, Train$HLag1PTInt, Train$HLag2LogInt, Train$HLag2PTInt, Train$HLag3LogInt, Train$HLag3PTInt, Train$HLag4LogInt, Train$HLag4PTInt, Train$HLag5LogInt, Train$HLag5PTInt))


# Data quality check
Temp <- ddply(training, .(Date), summarize, Count = length(Hour))
ggplot(NULL, aes(x = names(table(Temp[, 2])), y = sort(table(Temp[, 2])))) + geom_bar(stat = "identity", fill = factor(names(table(Temp[, 2]))))

## Holiday and HLagDelay are near zero variance variables, so the quality is decent
nearZeroVar(Train[, c(8:51)], saveMetrics = T)
## Temp is highly correlated with ATemp.
round(cor(Train[, c(12:16)]), 2)
findCorrelation(cor(Train[, c(12:16)]), cutoff = 0.75, verbose = T)
## No linear combination is found
findLinearCombos(Train[, c(12:51)][complete.cases(Train[, c(12:51)]), ])


# Build models
FormulaLog <- formula(CountLog ~ DsLog + Hour + Weekday + Season + Holiday + Workday + Weather + TempLog + ATempLog + HumidityLog + WindSpeedLog + HLag1CountLog + HLag1Delay + HLag1LogInt + HLag2CountLog + HLag2Delay + HLag2LogInt + HLag3CountLog + HLag3Delay + HLag3LogInt + HLag4CountLog + HLag4Delay + HLag4LogInt + HLag5CountLog + HLag5Delay + HLag5LogInt)
FormulaPT <- formula(CountPT ~ DsPT + Hour + Weekday + Season + Holiday + Workday + Weather + TempPT + ATemp + Humidity + WindSpeedPT + HLag1CountPT + HLag1Delay + HLag1PTInt + HLag2CountPT + HLag2Delay + HLag2PTInt + HLag3CountPT + HLag3Delay + HLag3PTInt + HLag4CountPT + HLag4Delay + HLag4PTInt + HLag5CountPT + HLag5Delay + HLag5PTInt)
TrainH5 <- Train[complete.cases(Train[, c("HLag1LogInt", "HLag2LogInt", "HLag3LogInt", "HLag4LogInt", "HLag5LogInt")]), ]
trainingFull <- rbind(data.frame(Train, Set = rep("Train", nrow(Train))), data.frame(Test[, !(names(Test) %in% c("ActualCount", "ActualCountLog", "ActualCountPT"))], Set = rep("Test", nrow(Test))))
trainingFull <- trainingFull[order(trainingFull$ID), ]

## Full Log Model, R-square: 93.7%, IS RMSLE: 0.356, OOS RMSLE: 0.6
LogFullModel <- lm(FormulaLog, data = TrainH5)
### In-Sample Prediction
LogFullPredIS <- predict(LogFullModel, newdata = TrainH5)
LogFullPredIS <- round(ifelse(exp(LogFullPredIS) - 1 < 0, 0, exp(LogFullPredIS) - 1))
(LogFullRMSLEIS <- RMSLE(LogFullPredIS, TrainH5[, "Count"]))
ggplot(NULL, aes(x = TrainH5[, "CountLog"], y = log(LogFullPredIS + 1))) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "In-Sample Prediction Plot (Log Transformation)", x = "Actual Count (Log) For Train Data", y = "Predicted Count (Log) For Train Data")
### Out-of-Sample Prediction
LogFullPredOOS <- PREDICT(LogFullModel, trainingFull, "Log")
LogFullPredOOS <- round(LogFullPredOOS)
(LogFullRMSLEOOS <- RMSLE(LogFullPredOOS, Test$ActualCount))
ggplot(NULL, aes(x = Test[, "ActualCountLog"], y = log(LogFullPredOOS + 1))) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "Out-of-Sample Prediction Plot (Log Transformation)", x = "Actual Count (Log) For Train Data", y = "Predicted Count (Log) For Train Data")

## Full PT Model, Adj R-square: 100%, IS RMSLE: 0.006, OOS RMSLE: 0.008
PTFullModel <- lm(FormulaPT, data = TrainH5)
### In-Sample Prediction
PTFullPredIS <- predict(PTFullModel, newdata = TrainH5)
PTFullPredIS <- round(ifelse(PTFullPredIS^(1/PT$roundlam[1]) - 1 < 0, 0, PTFullPredIS^(1/PT$roundlam[1]) - 1))
(PTFullRMSLEIS <- RMSLE(PTFullPredIS, TrainH5[, "Count"]))
ggplot(NULL, aes(x = TrainH5[, "CountPT"], y = (PTFullPredIS + 1)^PT$roundlam[1])) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "In-Sample Prediction Plot (PT Transformation)", x = "Actual Count (PT) For Train Data", y = "Predicted Count (PT) For Train Data")
### Out-of-Sample Prediction
PTFullPredOOS <- PREDICT(PTFullModel, trainingFull, "PT")
PTFullPredOOS <- round(PTFullPredOOS)
(PTFullRMSLEOOS <- RMSLE(PTFullPredOOS, Test$ActualCount))
ggplot(NULL, aes(x = Test[, "ActualCountPT"], y = (PTFullPredOOS + 1)^PT$roundlam[1])) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "Out-of-Sample Prediction Plot (PT Transformation)", x = "Actual Count (PT) For Train Data", y = "Predicted Count (PT) For Train Data")
ggplot(NULL, aes(x = Test[, "ActualCount"], y = PTFullPredOOS - Test[, "ActualCount"])) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "Out-of-Sample Prediction Plot (PT Transformation)", x = "Actual Count For Train Data", y = "Predicted Count For Train Data")


## Stepwise Log Model, IS RMSLE: 0.356, OOS RMSLE: 0.626
LogNullModel <- lm(CountLog ~ 1, data = TrainH5)
LogStepModel <- step(LogNullModel, scope = FormulaLog, dir = "both")
### In-Sample Prediction
LogStepPredIS <- predict(LogStepModel, newdata = TrainH5)
LogStepPredIS <- round(ifelse(exp(LogStepPredIS) - 1 < 0, 0, exp(LogStepPredIS) - 1))
(LogStepRMSLEIS <- RMSLE(LogStepPredIS, TrainH5[, "Count"]))
ggplot(NULL, aes(x = TrainH5[, "CountLog"], y = log(LogStepPredIS + 1))) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "In-Sample Prediction Plot (Log Transformation)", x = "Actual Count (Log) For Train Data", y = "Predicted Count (Log) For Train Data")
### Out-of-Sample Prediction
LogStepPredOOS <- PREDICT(LogStepModel, trainingFull, "Log")
LogStepPredOOS <- round(LogStepPredOOS)
(LogStepRMSLEOOS <- RMSLE(LogStepPredOOS, Test$ActualCount))
ggplot(NULL, aes(x = Test[, "ActualCountLog"], y = log(LogStepPredOOS + 1))) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "Out-of-Sample Prediction Plot (Log Transformation)", x = "Actual Count (Log) For Train Data", y = "Predicted Count (Log) For Train Data")

## Stepwise PT Model, IS RMSLE: 0.006, OOS RMSLE: 0.008
PTNullModel <- lm(CountPT ~ 1, data = TrainH5)
PTStepModel <- step(PTNullModel, scope = FormulaPT, dir = "both")
### In-Sample Prediction
PTStepPredIS <- predict(PTStepModel, newdata = TrainH5)
PTStepPredIS <- round(ifelse(PTStepPredIS^(1/PT$roundlam[1]) - 1 < 0, 0, PTStepPredIS^(1/PT$roundlam[1]) - 1))
(PTStepRMSLEIS <- RMSLE(PTStepPredIS, TrainH5[, "Count"]))
ggplot(NULL, aes(x = TrainH5[, "Count"], y = PTStepPredIS)) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "In-Sample Prediction Plot (Log Transformation)", x = "Actual Count For Train Data", y = "Predicted Count For Train Data")
### Out-of-Sample Prediction
PTStepPredOOS <- PREDICT(PTStepModel, trainingFull, "PT")
PTStepPredOOS <- round(PTStepPredOOS)
(PTStepRMSLEOOS <- RMSLE(PTStepPredOOS, Test$ActualCount))
ggplot(NULL, aes(x = Test[, "ActualCount"], y = PTStepPredOOS)) + geom_point() + stat_smooth(method = "glm", se = T, level = 0.95) + labs(title = "Out-of-Sample Prediction Plot (PT Transformation)", x = "Actual Count For Train Data", y = "Predicted Count For Train Data")



## ------------------Do Not Run Code Below

## Lasso PT Model, Minimal IS RMSLE: 0.112, which is at 98th lambda, so it's pretty much useless.
TrainH5Dummy <- model.matrix(FormulaPT, data = TrainH5)[, -1]
PTLassoModel <- glmnet(TrainH5Dummy, TrainH5[, "CountPT"])
PTLassoPred <- predict(PTLassoModel, newx = TrainH5Lasso)
PTLassoPred <- round(ifelse(PTLassoPred^(1/PT$roundlam[1]) - 1 < 0, 0, PTLassoPred^(1/PT$roundlam[1]) - 1))
PTLassoRMSLE <- apply(PTLassoPred, 2, RMSLE, Reference = TrainH5[, "Count"])
which.min(PTLassoRMSLE)


### CV Lasso, IS RMSLE: 0.116
PTLassoCVModel <- cv.glmnet(TrainH5Lasso, TrainH5[, "CountPT"], nfolds = 10)
PTLassoCVPred <- predict(PTLassoCVModel, newx = TrainH5Lasso)
PTLassoCVPred <- round(ifelse(PTLassoCVPred^(1/PT$roundlam[1]) - 1 < 0, 0, PTLassoCVPred^(1/PT$roundlam[1]) - 1))
PTLassoCVPred <- as.vector(PTLassoCVPred)
(PTLassoCVRMSLE <- RMSLE(PTLassoCVPred, TrainH5[, "Count"]))
PTLassoCVPred


