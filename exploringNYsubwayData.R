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

ggplot(d, aes(hour, ENTRIESn_hourly)) + geom_boxplot(color="gray30") + coord_cartesian(ylim=c(0, 5000)) + 
  theme(plot.title = element_text(size=20, face="bold", vjust=3, color="deepskyblue4"), 
        axis.text.x = element_text(size=14, angle = 0, hjust = 1, color="deepskyblue4"), 
        axis.text.y = element_text(size=14, color="deepskyblue4"),
        axis.title.x = element_text(color="deepskyblue4", size=16), 
        axis.title.y = element_text(color="deepskyblue4", size=16, vjust=1), 
        legend.title=element_blank(), 
        legend.text = element_text(colour="deepskyblue4", size = 18)
        ) + 
  xlab("Hour of day (0 is midnight)") +
  ggtitle("\nEntries per hour by time of day")


## Odd hours have very few people using the subway.

ggplot(d, aes(weekday, ENTRIESn_hourly)) + geom_boxplot() + coord_cartesian(ylim=c(0, 5000)) ## On weekends fewer people using the subway.

ggplot(d, aes(fog, ENTRIESn_hourly)) + geom_boxplot() + coord_cartesian(ylim=c(0, 5000)) ## Foggy condition has less entries per hour.

ggplot(d, aes(latitude, longitude)) + geom_point(aes(size=ENTRIESn_hourly, color=ENTRIESn_hourly)) + 
  theme(plot.title = element_text(size=20, face="bold", vjust=3, color="deepskyblue4"), 
        axis.text.x = element_text(size=14, angle = 0, hjust = 1, color="deepskyblue4"), 
        axis.text.y = element_text(size=14, color="deepskyblue4"),
        axis.title.x = element_text(color="deepskyblue4", size=16), 
        axis.title.y = element_text(color="deepskyblue4", size=16, vjust=1), 
        legend.title=element_blank(), 
        legend.text = element_text(colour="deepskyblue4", size = 18)
  ) + 
  xlab("latitude") + ylab("longitude") +
  ggtitle("\nMapping locations of stations with high number of entries per hour.")

# Where are the stations with most entries?

## I need to create a table with all UNITs with their median ENTRIESn_hourly, max ENTRIESn_hourly, and total entries per day. 
## Then I can see which UNITs have the highest traffic and where they are located (by plotting them on map of NY.)

summary(lm(ENTRIESn_hourly ~ rain, data = d))
summary(lm(ENTRIESn_hourly ~ rain + fog, data = d))
summary(lm(ENTRIESn_hourly ~ rain + weekday, data = d))
summary(lm(ENTRIESn_hourly ~ rain + hour + weekday, data = d))


summary(lm(ENTRIESn_hourly ~ UNIT, data = d))[9]
summary(lm(ENTRIESn_hourly ~ EXITSn, data = d))[9]
summary(lm(ENTRIESn_hourly ~ hour, data = d))[9]
summary(lm(ENTRIESn_hourly ~ day_week, data = d))[9]
summary(lm(ENTRIESn_hourly ~ wspdi, data = d))[9]
summary(lm(ENTRIESn_hourly ~ tempi, data = d))[9]
summary(lm(ENTRIESn_hourly ~ precipi, data = d))[9] 
summary(lm(ENTRIESn_hourly ~ meanprecipi, data = d))[9] 
summary(lm(ENTRIESn_hourly ~ pressurei, data = d))[9]
summary(lm(ENTRIESn_hourly ~ meantempi, data = d))[9]
summary(lm(ENTRIESn_hourly ~ rain, data = d))[9]
summary(lm(ENTRIESn_hourly ~ fog, data = d))[9]


plot(lm(ENTRIESn_hourly ~ rain, data = d))


rainsummary(lm(ENTRIESn_hourly ~ UNIT+EXITSn+hour+wspdi+tempi+precipi+rain+fog, data = d))

  
  
## 