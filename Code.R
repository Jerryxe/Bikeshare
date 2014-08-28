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
training <- data.frame(training[, c(12, 16)], training[, 1:8], training[, 13:15], training[, 9:11])
names(training) <- c("DateTime", "Date", "Season", "Holiday", "Workday", "Weather", "Temp", "ATemp", "Humidity", "WindSpeed", "Hour", "Weekday", "T", "Casual", "Registered", "Count")
training$DateTime <- as.POSIXlt(training$DateTime)

# Exploratory Analysis
## Plot Autocorrelation Function (ACF), and we could see:
## 1. There is autocorrelations for about 5 periods (will dig deeper).
## 2. There is alternative dependence with about 24 periods interval (presumably means 24 hours).
plot(acf(training$count, lag.max = 100))

## Plot ACF for each hour:
## 1. Now we could see there is alternative dependence for about 7 periods (presumably means 7 days).
plot(acf(training$count[training$Hour == 12], lag.max= 50))

## Plot ACF for both hour and weekday, and here we check few different day and hour:
## 1. There is almost no alternative dependence
plot(acf(training$count[training$Hour == 12 & training$Weekday == "Saturday"]))
plot(acf(training$count[training$Hour == 16 & training$Weekday == "Tuesday"]))


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



# Build models
## 








# Evaluation Formula (Root Mean Squared Logarithmic Error):
RMSLE <- function(Predicted, Reference) {
    n <- length(Predicted)
    sqrt(sum((log(Predicted + 1) - log(Reference + 1))^2)/n)
}





