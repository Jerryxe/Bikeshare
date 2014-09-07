Y2011Q1 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2011-1st-quarter.csv", header = T, stringsAsFactors = F)
Y2011Q2 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2011-2nd-quarter.csv", header = T, stringsAsFactors = F)
Y2011Q3 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2011-3rd-quarter.csv", header = T, stringsAsFactors = F)
Y2011Q4 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2011-4th-quarter.csv", header = T, stringsAsFactors = F)
Y2012Q1 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2012-1st-quarter.csv", header = T, stringsAsFactors = F)
Y2012Q2 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2012-2nd-quarter.csv", header = T, stringsAsFactors = F)
Y2012Q3 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2012-3rd-quarter.csv", header = T, stringsAsFactors = F)
Y2012Q4 <- read.csv("/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/2012-4th-quarter.csv", header = T, stringsAsFactors = F)
Y2012Q1 <- data.frame(Y2012Q1[, 1], Y2012Q1[, 3], Y2012Q1[, 6], Y2012Q1[, 4], Y2012Q1[, 7], Y2012Q1[, 9:10])
names(Y2012Q1) <- names(Y2011Q1)
Y2012Q2 <- data.frame(Y2012Q2[, 1], Y2012Q2[, 3], Y2012Q2[, 6], Y2012Q2[, 4], Y2012Q2[, 7], Y2012Q2[, 9:10])
names(Y2012Q2) <- names(Y2011Q1)
Y2012Q3 <- data.frame(Y2012Q3[, 1], Y2012Q3[, 2], Y2012Q3[, 5], Y2012Q3[, 3], Y2012Q3[, 6], Y2012Q3[, 8:9])
names(Y2012Q3) <- names(Y2011Q1)
Y2012Q4 <- data.frame(Y2012Q4[, 1], Y2012Q4[, 2], Y2012Q4[, 5], Y2012Q4[, 3], Y2012Q4[, 6], Y2012Q4[, 8:9])
names(Y2012Q4) <- names(Y2011Q1)

Rental <- rbind(Y2011Q1, Y2011Q2, Y2011Q3, Y2011Q4, Y2012Q1, Y2012Q2, Y2012Q3, Y2012Q4)
Rental$StartDateTime <- strptime(Rental$Start.date, format = "%m/%d/%Y %H", tz = "GMT")
Rental$ID <- 1:nrow(Rental)
Rental$StartDateTime1 <- factor(as.character(Rental$StartDateTime))
Count <- sapply(split(Rental$ID, Rental$StartDateTime1), length)
Count <- data.frame(DateTime = names(Count), Count)

training <- read.csv("train.csv", header = T, stringsAsFactors = F)
training$DateTime <- strptime(training$datetime, format = "%Y-%m-%d %H", tz = "GMT")

training$DateTime1 <- factor(as.character(training$DateTime))
Count1 <- Count[Count$DateTime %in% training$DateTime1, ]
A <- merge(training[, c("DateTime1", "count")], Count1, by.x = "DateTime1", by.y = "DateTime")
sum(A$count == A$Count)    ## check if it's 10886



testing <- read.csv("test.csv", header = T, stringsAsFactors = F)
testing$DateTime <- strptime(testing$datetime, format = "%Y-%m-%d %H", tz = "GMT")
testing$DateTime1 <- factor(as.character(testing$DateTime))
Count2 <- Count[Count$DateTime %in% testing$DateTime1, ]
testing <- merge(testing, Count2, by.x = "DateTime1", by.y = "DateTime")
testing$DateTime1 <- NULL
testing$DateTime <- NULL
names(testing) <- c(names(testing)[1:(length(names(testing)) - 1)], "count")

write.csv(testing, file = "/Users/Jerry/Dropbox/Education/Data Analytics/BriLent Analytics Bootcamp/Bikeshare/test1.csv", row.names = F)
