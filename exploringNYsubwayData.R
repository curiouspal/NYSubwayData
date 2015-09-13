library("ggplot2")

d <- read.csv("turnstile_weather_v2.csv")


d$hour <- as.factor(d$hour)
d$day_week <- as.factor(d$day_week)
d$weekday <- as.factor(d$weekday)
d$station <- as.factor(d$station)
d$fog <- as.factor(d$fog)
d$precipi <- as.ordered(d$precipi)
d$rain <- as.factor(d$rain)
d$wspdi <- as.ordered(d$wspdi)

summary(d)


summary(d$ENTRIESn)
ggplot(d, aes(d$ENTRIESn_hourly)) + geom_histogram(binwidth=100)
ggplot(d, aes(d$tempi, d$ENTRIESn_hourly)) + geom_point()
ggplot(d, aes(d$UNIT, d$ENTRIESn_hourly)) + geom_boxplot()

ggplot(d, aes(as.factor(rain), ENTRIESn_hourly)) + geom_boxplot()

ggplot(d, aes(hour, ENTRIESn_hourly)) + geom_boxplot() + coord_cartesian(ylim=c(0, 5000)) ## Odd hours have very few people using the subway.

ggplot(d, aes(weekday, ENTRIESn_hourly)) + geom_boxplot() + coord_cartesian(ylim=c(0, 5000)) ## On weekends fewer people using the subway.

ggplot(d, aes(fog, ENTRIESn_hourly)) + geom_boxplot() + coord_cartesian(ylim=c(0, 5000)) ## Foggy condition has less entries per hour.


## I need to create a table with all UNITs with their median ENTRIESn_hourly, max ENTRIESn_hourly, and total entries per day. 
## Then I can see which UNITs have the highest traffic and where they are located (by plotting them on map of NY.)

## 