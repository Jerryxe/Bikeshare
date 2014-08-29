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
training$Hour <- factor(training$DateTime$hour)
training$Weekday <- factor(weekdays(training$DateTime))
training$t <- as.numeric(as.Date(training$DateTime) - as.Date(training$DateTime[1])) + 1
training$season <- factor(training$season)
training$holiday <- factor(training$holiday)
training$workingday <- factor(training$workingday)
training$weather <- factor(training$weather)
training$Date <- as.Date(training$DateTime)
training$ID <- 1:length(training$Date)
training <- data.frame(training[, 17], training[, c(12, 16)], training[, 1:8], training[, 13:15], training[, 9:11])
names(training) <- c("ID", "DateTime", "Date", "Season", "Holiday", "Workday", "Weather", "Temp", "ATemp", "Humidity", "WindSpeed", "Hour", "Weekday", "T", "Casual", "Registered", "Count")
training <- training[order(training$DateTime), ]
training$DateTime <- as.POSIXlt(training$DateTime)

# Evaluation Formula (Root Mean Squared Logarithmic Error):
RMSLE <- function(Predicted, Reference) {
    n <- length(Predicted)
    sqrt(sum((log(Predicted + 1) - log(Reference + 1))^2)/n)
}

# Exploratory Analysis
## Plot Autocorrelation Function (ACF), and we could see:
## 1. There is autocorrelations for about 5 periods (will dig deeper).
## 2. There is alternative dependence with about 24 periods interval (presumably means 24 hours).
plot(acf(training$Count, lag.max = 100))

## Plot ACF for each hour:
## 1. Now we could see there is alternative dependence for about 7 periods (presumably means 7 days in a week).
plot(acf(training$Count[training$Hour == 12], lag.max= 50))

## Plot ACF for both hour and weekday, and here we check few different day and hour:
## 1. There is almost no alternative dependence
plot(acf(training$Count[training$Hour == 12 & training$Weekday == "Saturday"]))
plot(acf(training$Count[training$Hour == 16 & training$Weekday == "Tuesday"]))


## Plot the cycle of 24 hours
### There is significant difference in each hour
ggplot(training, aes(x = Hour, y = Count)) + geom_boxplot(aes(fill = Hour)) + scale_fill_hue(guide = F)
### Plot the Jan 2011 rent, and we could see there is claer cycle
ggplot(training[training$DateTime$year == 111 & training$DateTime$mon == 0, ], aes(x = DateTime, y = Count)) + geom_point() + geom_line()
### Plot the first 5 days of Jan 2011 to have a closer look, and we could see for each day, there are 2 peaks (around 8AM and 6PM), and there is also a low time at the end of each day.
ggplot(training[training$DateTime$year == 111 & training$DateTime$mon == 0 & training$DateTime$yday <= 5, ], aes(x = DateTime, y = Count)) + geom_point() + geom_line()


## Plot the cycle of 7 days in a week
### Plot the rent of 8AM for each different weekday. Apparently, there is significant different between weekdays and weekends.
ggplot(training[training$Hour == 8, ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Weekday))
### Workday plot also shows this pattern.
ggplot(training[training$Hour == 8, ], aes(x = DateTime, y = Count)) + geom_line(aes(color = Workday))


# Preapring T-Series Data
## Add T-1, T-2, T-3 Count Data based on Weekday and Hour
WeekdayList <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
HourList <- as.character(0:23)
training2 <- training[0, ]
for (i in 1:7) {
    for (j in 1:24) {
        Temp <- training[training$Weekday == WeekdayList[i] & training$Hour == HourList[j], ]
        Temp$Tm1Count <- c(NA, Temp$Count[1:(length(Temp$Count) - 1)])
        Temp$Tm1Delay <- c(NA, Temp$T[2:length(Temp$T)] - Temp$T[1:(length(Temp$T) - 1)])/7
        Temp$Tm2Count <- c(NA, NA, Temp$Count[1:(length(Temp$Count)-2)])
        Temp$Tm2Delay <- c(NA, NA, Temp$T[3:length(Temp$T)] - Temp$T[1:(length(Temp$T) - 2)])/7
        Temp$Tm3Count <- c(NA, NA, NA, Temp$Count[1:(length(Temp$Count)-3)])
        Temp$Tm3Delay <- c(NA, NA, NA, Temp$T[4:length(Temp$T)] - Temp$T[1:(length(Temp$T) - 3)])/7
        training2 <- rbind(training2, Temp)
    }
}
training <- training2[order(training2$ID), ]
training$Tm1Mix <- training$Tm1Count * training$Tm1Delay
training$Tm2Mix <- training$Tm2Count * training$Tm2Delay
training$Tm3Mix <- training$Tm3Count * training$Tm3Delay
rm(training2, i, j)


# Data quality check
## Holiday is a near zero variance, but the rest are not, so the quality is decent
nearZeroVar(training[, c(4:13, 15:26)], saveMetrics = T)
## Temp is highly correlated with ATemp, Registered is highly correlated with Count.
round(cor(training[, c(8:11, 15:17)]), 2)
findCorrelation(cor(training[, c(8:11, 15:17)]), cutoff = 0.75, verbose = T)
## No linear combination is found
findLinearCombos(training[, c(8:11, 18:26)][complete.cases(training[, c(24:26)]), ])


# Build models with interactions and 3-period autocorrelation AR(3)
Formula <- formula(Count ~ Season + Holiday + Workday + Weather + Temp + ATemp + Humidity + WindSpeed + Hour + Weekday + Tm1Count + Tm1Delay + Tm1Mix + Tm2Count + Tm2Delay + Tm2Mix + Tm3Count + Tm3Delay + Tm3Mix)
trainingTm3 <- training[complete.cases(training[, c("Tm1Mix", "Tm2Mix", "Tm3Mix")]), ]

## Linear gaussian model. In-sample performance: R2 is 88%, and RMSLE is 0.76
lmModel <- train(Formula, data = trainingTm3, method = "lm")
lmPred <- predict(lmModel, newdata = trainingTm3)
lmPred <- ifelse(lmPred < 0, 0, round(qrPred))
(lmRMSLE <- RMSLE(lmPred, trainingTm3[, "Count"]))
### From the residual plot we could see there is non-constant variance in the residual, so we need to do the transformation.
ggplot(NULL, aes(x = lmPred, y = trainingTm3[, "Count"] - lmPred)) + geom_point()

### With the log transformation of Count, Tm1Count, Tm2Count, Tm3Count, RMSLE is reduced to 0.38 from 0.76
trainingTm3$Tm1CountLog <- log(trainingTm3$Tm1Count)
trainingTm3$Tm2CountLog <- log(trainingTm3$Tm2Count)
trainingTm3$Tm3CountLog <- log(trainingTm3$Tm3Count)
trainingTm3$Tm1MixLog <- trainingTm3$Tm1CountLog * trainingTm3$Tm1Delay
trainingTm3$Tm2MixLog <- trainingTm3$Tm2CountLog * trainingTm3$Tm2Delay
trainingTm3$Tm3MixLog <- trainingTm3$Tm3CountLog * trainingTm3$Tm3Delay
lm2Model <- train(log(Count) ~ Season + Holiday + Workday + Weather + Temp + ATemp + Humidity + WindSpeed + Hour + Weekday + Tm1CountLog + Tm1Delay + Tm1MixLog + Tm2CountLog + Tm2Delay + Tm2MixLog + Tm3CountLog + Tm3Delay + Tm3MixLog, data = trainingTm3, method = "lm")
lm2Pred <- predict(lm2Model, newdata = trainingTm3)
(lm2RMSLE <- RMSLE(round(exp(lm2Pred)), trainingTm3[, "Count"]))
### Now the residuals have constant variance
ggplot(NULL, aes(x = lm2Pred, y = log(trainingTm3$Count) - lm2Pred)) + geom_point()



## poisson model. It didn't work well, RMSLE is 3.15
poisModel <- glm(Formula, data = trainingTm3, family = "poisson")
poisPred <- predict(poisModel, newdata = trainingTm3)
qrRMSLE <- RMSLE(round(poisPred), trainingTm3[, "Count"])

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


