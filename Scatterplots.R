library(ggplot2)

#Read in the "Sleep Efficiency" data set
path<-""
dataset<-read.csv(path)

#Format bedtimes as POSIX
#The sample points are spread out across several dates
#In order to graph them in the same 24-hour period, assign them all the same "dummy" date
#Note that bedtimes after midnight get assigned to the next day
bedtimes<-sapply(strsplit(dataset$Bedtime," "), `[`, 2)
formattedBedtimes<-c()
for(bedtime in bedtimes) {
  date<-"01/01/2023"
  if (strtoi(sapply(strsplit(bedtime,":"),`[`,1)) < 12) {
    date<-"01/02/2023"
  }
  formattedBedtimes<-append(formattedBedtimes,as.POSIXct(paste(date, bedtime),format="%m/%d/%Y %H:%M"))
}

#Grab other columns of interest from the data set
sleepDuration<-dataset$Sleep.duration
sleepEfficiency<-dataset$Sleep.efficiency
REMSleep<-dataset$REM.sleep.percentage

#Calculate quantities of interest
actualSleepDuration<-sleepDuration*sleepEfficiency

#Scatter plot of bedtime vs. (actual) sleep duration
df<-data.frame(formattedBedtimes, actualSleepDuration)
# plot x values in 24 hour format
ggplot(df, aes(x = formattedBedtimes, y = actualSleepDuration)) +
  geom_point() +
  scale_x_datetime(date_labels = "%H:%M", limits = as.POSIXct(c("01/01/2023 20:30", "01/02/2023 3:00"), format="%m/%d/%Y %H:%M")) +
  labs(x = "Bedtime (24-Hour Day)", y = "Sleep Duration (Hours)")

plot.new()

#Scatter plot of bedtime vs. % REM sleep
df<-data.frame(formattedBedtimes, REMSleep)
# plot x values in 24 hour format
ggplot(df, aes(x = formattedBedtimes, y = REMSleep)) +
  geom_point() +
  scale_x_datetime(date_labels = "%H:%M", limits = as.POSIXct(c("01/01/2023 20:30", "01/02/2023 3:00"), format="%m/%d/%Y %H:%M")) +
  labs(x = "Bedtime (24-Hour Day)", y = "Percent REM Sleep")

