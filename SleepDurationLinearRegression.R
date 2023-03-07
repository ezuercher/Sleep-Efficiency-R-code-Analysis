#Load libraries
library(ggplot2)
library(tidyverse)
library(ggpmisc)
font <- "Arial Unicode MS"

#Read in the "Sleep Efficiency" data set
path<-""
dataset<-data.frame(read.csv(path))

#Sort the sleep durations by bedtime
nine<-c()
ninethirty<-c()
ten<-c()
tenthirty<-c()
eleven<-c()
twelve<-c()
twelvethirty<-c()
one<-c()
onethirty<-c()
two<-c()
twothirty<-c()
for(row in 1:nrow(dataset)) {
  bedtime<-sapply(strsplit(dataset[row,"Bedtime"]," "), `[`, 2)
  if (bedtime == "21:00") {
    nine<-append(nine, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "21:30") {
    ninethirty<-append(ninethirty, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "22:00") {
    ten<-append(ten, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "22:30") {
    tenthirty<-append(tenthirty, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "23:00") {
    eleven<-append(eleven, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "0:00") {
    twelve<-append(twelve, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "0:30") {
    twelvethirty<-append(twelvethirty, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "1:00") {
    one<-append(one, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "1:30") {
    onethirty<-append(onethirty, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "2:00") {
    two<-append(two, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  } else if (bedtime == "2:30") {
    twothirty<-append(twothirty, dataset[row,"Sleep.duration"] * dataset[row,"Sleep.efficiency"])
  }
}

#Calculate the mean sleep duration for each bedtime
meanSleepDuration<-c(mean(nine),mean(ninethirty),mean(ten),mean(tenthirty),mean(eleven),
                  mean(twelve),mean(twelvethirty),mean(one),mean(onethirty),mean(two),mean(twothirty))

#Format the mean sleep durations in a data frame
uniqueBedtimes<-c("21:00","21:30","22:00","22:30","23:00","0:00","0:30","1:00","1:30","2:00","2:30")
meanSleepDurationDf<-data.frame(
  Bedtime = uniqueBedtimes,
  Mean_Sleep_Time = meanSleepDuration
)
meanSleepDurationDf

#Format the unique bedtimes as POSIX
#In order to graph them in the same 24-hour period, assign them all the same "dummy" date
#Note that bedtimes after midnight get assigned to the next day
formattedBedtimes<-c()
for(bedtime in uniqueBedtimes) {
  date<-"01/01/2023"
  if (strtoi(sapply(strsplit(bedtime,":"),`[`,1)) < 12) {
    date<-"01/02/2023"
  }
  formattedBedtimes<-append(formattedBedtimes,as.POSIXct(paste(date, bedtime),format="%m/%d/%Y %H:%M"))
}

#Display scatter plot of bedtime vs. mean sleep duration
#Include the linear model on the graph
df<-data.frame(formattedBedtimes, meanSleepDuration)
#Create a linear model of the data
meanSleepDuration.lm<-lm(meanSleepDuration~formattedBedtimes,data=df)
summary(meanSleepDuration.lm)
# plot x values in 24 hour format
ggplot(df, aes(x = formattedBedtimes, y = meanSleepDuration)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~~")),parse=TRUE,label.y=0.8,hjust=0,vjust=1.0,family=font) +
  scale_x_datetime(date_labels = "%H:%M", limits = as.POSIXct(c("01/01/2023 20:30", "01/02/2023 3:00"), format="%m/%d/%Y %H:%M")) +
  labs(x = "Bedtime (24-Hour Day)", y = "Mean Sleep Duration (Hours)")