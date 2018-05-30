# Load packages to use
require(xlsx)
require(ggplot2)

# Load file from questionnaires into data frams
eyeQ <- read.xlsx("C:\\DTU\\Data\\201805_HealthnRehab\\Questionnaire\\Book1.xlsx",1, header = TRUE)

# Pie chart to show gender distribution
nFem = length(eyeQ$Gender[eyeQ$Gender=="Female"])
nMal = length(eyeQ$Gender[eyeQ$Gender=="Male"])
sliceGen <-  c(nMal, nFem)
pieGenNames <- c("Male", "Female")
pctGen = round(sliceGen/sum(sliceGen)*100)
pieGenNames <- paste(pieGenNames, pctGen, sep= " ")
pieGenNames <- paste(pieGenNames, '%', sep= " ")
pie(sliceGen, labels = pieGenNames, main = "Eye typing experiment: Gender distribution")


# Pie chart to show gender distribution
nDT = length(eyeQ$Typing.mechanism[eyeQ$Typing.mechanism=="DT"])
nMS = length(eyeQ$Typing.mechanism[eyeQ$Typing.mechanism=="MS"])
sliceTM <-  c(nDT, nMS)
pieTMNames <- c("Dwell-time", "Multi-swipe")
pctTM = round(sliceTM/sum(sliceTM)*100)
pieTMNames <- paste(pieTMNames, pctTM, sep= " ")
pieTMNames <- paste(pieTMNames, '%', sep= " ")
pie(sliceTM, labels = pieTMNames, main = "Eye typing experiment: Typing mechanism distribution")


# Pie chart on gaze interaction experience
nNever = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Never"])
nMultiple = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Multiple times"])
nOnce = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Once"])
sliceGazeExp <- c(nNever, nOnce, nMultiple)
pieGazeExpNames <- c("Never", "Once", "Multiple times")
pctGazeExp = round(sliceGazeExp/sum(sliceGazeExp)*100)
pieGazeExpNames <- paste(pieGazeExpNames, pctGazeExp, sep = " ")
pieGazeExpNames <- paste(pieGazeExpNames, '%', sep = " ")
pie(sliceGazeExp, labels = pieGazeExpNames, main = "Eye typing experiment: Gaze interaction experience")


# Histogram on score on comfortableness
hist(eyeQ$How.comfortable.was.it.during.the.task.[eyeQ$How.comfortable.was.it.during.the.task.!=0], main = "How comfortable was the task?", xlab = "Score", xlim = c(1,10))
hist(eyeQ$How.challenging.was.the.task.[eyeQ$How.challenging.was.the.task.!=0], main = "How challenging was the task?", xlab = "Score", xlim = c(1,10))
hist(eyeQ$How.much.fun.was.the.task.[eyeQ$How.much.fun.was.the.task.!=0], main = "How much fun was the task?", xlab = "Score", xlim = c(1,10))


# Histogram on typing speed in wpm
p1 <- hist(eyeQ$Typing.Speed[eyeQ$Typing.mechanism=="DT"], main = "Typing speeds for dwell-time typing", xlab = "Typing speed (in wpm)")
p2 <- hist(eyeQ$Typing.Speed[eyeQ$Typing.mechanism=="MS"], xlab = "Typing speed (in wpm)")

plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,14), main = "Typing speed distribution", xlab = "Typing speed [in wpm]")  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,14), add=T)  # second
legend(11,7,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))


# Histogram on error rate
MSDwoZero <- subset(eyeQ$MSD.error.rate, eyeQ$MSD.error.rate!=0.00)
p3 <- hist(eyeQ$MSD.error.rate[eyeQ$Typing.mechanism=="DT"])
p4 <- hist(eyeQ$MSD.error.rate[eyeQ$Typing.mechanism=="MS"])

plot( p3, col=rgb(0,0,1,1/4), xlim=c(0,100), ylim =c(0,10), main = "Error rate distribution", xlab = "Error rate")  # first histogram
plot( p4, col=rgb(1,0,0,1/4), xlim=c(0,100), ylim =c(0,10), add=T)  # second
legend(80,10,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))


