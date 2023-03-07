#Read in the "Sleep Efficiency" data set
path<-""
dataset<-data.frame(read.csv(path))

#Compute the overall average sleep duration
sleepDuration<-dataset$Sleep.duration
sleepEfficiency<-dataset$Sleep.efficiency
actualSleepDuration<-sleepDuration*sleepEfficiency
averageSleepDuration<-mean(actualSleepDuration)
averageSleepDuration

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

#Group the bedtimes into a list for convenient access
uniqueBedtimes<-list(
  nine,
  ninethirty,
  ten,
  tenthirty,
  eleven,
  twelve,
  twelvethirty,
  one,
  onethirty,
  two,
  twothirty
)

#Create a data frame to store our results
aboveAverageProbDf<-data.frame(
  Bedtime = c("21:00","21:30","22:00","22:30","23:00","0:00","0:30","1:00","1:30","2:00","2:30"),
  Probability = c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
)

#Begin computing the conditional probabilities:
#P(Above Average Sleep Duration | Bedtime)
#Use the formula for conditional probability;
#P(A|B)=P(A&B)/P(B)
counter<-1
for(uniqueBedtime in uniqueBedtimes) {
  bedtimeAndAboveAverage<-0
  for (duration in uniqueBedtime) {
    if (duration >= averageSleepDuration) {
      bedtimeAndAboveAverage<-bedtimeAndAboveAverage+1
    }
  }
  aboveAverageGivenBedtime<-(bedtimeAndAboveAverage/length(uniqueBedtime))
  aboveAverageProbDf[counter,2]<-aboveAverageGivenBedtime
  counter<-counter+1
}

#Display the data frame
aboveAverageProbDf
