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

# Pie chart to show gender distribution - DT
nFem = length(eyeQ$Gender[eyeQ$Gender=="Female" & eyeQ$Typing.mechanism=='DT'])
nMal = length(eyeQ$Gender[eyeQ$Gender=="Male" & eyeQ$Typing.mechanism=='DT'])
sliceGen <-  c(nMal, nFem)
pieGenNames <- c("Male", "Female")
pctGen = round(sliceGen/sum(sliceGen)*100)
pieGenNames <- paste(pieGenNames, pctGen, sep= " ")
pieGenNames <- paste(pieGenNames, '%', sep= " ")
pie(sliceGen, labels = pieGenNames, main = "Eye typing experiment: Gender distribution for dwell time typing")

# Pie chart to show gender distribution - MS
nFem = length(eyeQ$Gender[eyeQ$Gender=="Female" & eyeQ$Typing.mechanism=='MS'])
nMal = length(eyeQ$Gender[eyeQ$Gender=="Male" & eyeQ$Typing.mechanism=='MS'])
sliceGen <-  c(nMal, nFem)
pieGenNames <- c("Male", "Female")
pctGen = round(sliceGen/sum(sliceGen)*100)
pieGenNames <- paste(pieGenNames, pctGen, sep= " ")
pieGenNames <- paste(pieGenNames, '%', sep= " ")
pie(sliceGen, labels = pieGenNames, main = "Eye typing experiment: Gender distribution for multi swipe typing")

# Age distribution
barplot(table(eyeQ$Age.bins))

# Historgram of typing speed vs gender
boxplot(eyeQ$Typing.Speed[eyeQ$Gender!='NA'] ~ eyeQ$Gender[eyeQ$Gender!='NA'], data = eyeQ, main = 'Typing speed and gender', xlab = 'Gender', ylab = 'Typing speed [in wpm]')

kruskal.test(eyeQ$Typing.Speed[eyeQ$Gender!='NA'] ~ eyeQ$Gender[eyeQ$Gender!='NA'])

# Histogram of typing speed vs age
AgeWoNone <- subset(eyeQ$Age.bins, eyeQ$Age.bins!='NA')
TypeSpeedWoNone <- subset(eyeQ$Typing.Speed, eyeQ$Age.bins!='NA')

qplot( AgeWoNone, TypeSpeedWoNone, xlab = 'Age', ylab = 'Typing speed [in wpm]', main = 'Typing speed and age')+
  geom_smooth(method = 'lm')

# Pie chart to show typing mechanism distribution
nDT = length(eyeQ$Typing.mechanism[eyeQ$Typing.mechanism=="DT"])
nMS = length(eyeQ$Typing.mechanism[eyeQ$Typing.mechanism=="MS"])
sliceTM <-  c(nDT, nMS)
pieTMNames <- c("Dwell-time", "Multi-swipe")
pctTM = round(sliceTM/sum(sliceTM)*100)
pieTMNames <- paste(pieTMNames, pctTM, sep= " ")
pieTMNames <- paste(pieTMNames, '%', sep= " ")
pie(sliceTM, labels = pieTMNames, main = "Eye typing experiment: Typing mechanism distribution")


# Pie chart on gaze interaction experience - Overall
nNever = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Never"])
nMultiple = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Multiple times"])
nOnce = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Once"])
sliceGazeExp <- c(nNever, nOnce, nMultiple)
pieGazeExpNames <- c("Never", "Once", "Multiple times")
pctGazeExp = round(sliceGazeExp/sum(sliceGazeExp)*100)
pieGazeExpNames <- paste(pieGazeExpNames, pctGazeExp, sep = " ")
pieGazeExpNames <- paste(pieGazeExpNames, '%', sep = " ")
pie(sliceGazeExp, labels = pieGazeExpNames, main = "Eye typing experiment: Gaze interaction experience")


# Pie chart on gaze interaction experience for DT
nNever = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Never" & eyeQ$Typing.mechanism=='DT'])
nMultiple = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Multiple times" & eyeQ$Typing.mechanism=='DT'])
nOnce = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Once" & eyeQ$Typing.mechanism=='DT'])
sliceGazeExp <- c(nNever, nOnce, nMultiple)
pieGazeExpNames <- c("Never", "Once", "Multiple times")
pctGazeExp = round(sliceGazeExp/sum(sliceGazeExp)*100)
pieGazeExpNames <- paste(pieGazeExpNames, pctGazeExp, sep = " ")
pieGazeExpNames <- paste(pieGazeExpNames, '%', sep = " ")
pie(sliceGazeExp, labels = pieGazeExpNames, main = "Eye typing experiment: Gaze interaction experience by users of dwell time typing")


# Pie chart on gaze interaction experience for MS
nNever = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Never" & eyeQ$Typing.mechanism=='MS'])
nMultiple = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Multiple times" & eyeQ$Typing.mechanism=='MS'])
nOnce = length(eyeQ$Gaze.Interaction.experience.[eyeQ$Gaze.Interaction.experience.=="Once" & eyeQ$Typing.mechanism=='MS'])
sliceGazeExp <- c(nNever, nOnce, nMultiple)
pieGazeExpNames <- c("Never", "Once", "Multiple times")
pctGazeExp = round(sliceGazeExp/sum(sliceGazeExp)*100)
pieGazeExpNames <- paste(pieGazeExpNames, pctGazeExp, sep = " ")
pieGazeExpNames <- paste(pieGazeExpNames, '%', sep = " ")
pie(sliceGazeExp, labels = pieGazeExpNames, main = "Eye typing experiment: Gaze interaction experience by users of multi swipe typing")


# Histogram on score on comfortableness - 
p1 <- hist(eyeQ$How.comfortable.was.it.during.the.task.[eyeQ$How.comfortable.was.it.during.the.task.!=0 & eyeQ$Typing.mechanism=='DT'], main = "How comfortable was the task?", xlab = "Score", breaks = c(0,1,2,3,4,5,6,7,8,9,10))
p2 <- hist(eyeQ$How.comfortable.was.it.during.the.task.[eyeQ$How.comfortable.was.it.during.the.task.!=0 & eyeQ$Typing.mechanism=='MS'], main = "How comfortable was the task?", xlab = "Score", breaks = c(0,1,2,3,4,5,6,7,8,9,10))
plot( p1, col=rgb(0,0,1,1/4), main = "Comfortableness score distribution", xlab = "Score")  # first histogram
plot( p2, col=rgb(1,0,0,1/4),add=T)  # second
legend(4,5,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))


p3 <- hist(eyeQ$How.challenging.was.the.task.[eyeQ$How.challenging.was.the.task.!=0 & eyeQ$Typing.mechanism=='DT'], main = "How challenging was the task?", xlab = "Score", breaks = c(0,1,2,3,4,5,6,7,8,9,10))
p4 <- hist(eyeQ$How.challenging.was.the.task.[eyeQ$How.challenging.was.the.task.!=0 & eyeQ$Typing.mechanism=='MS'], main = "How challenging was the task?", xlab = "Score", xlim = c(1,10), breaks = c(0,1,2,3,4,5,6,7,8,9,10))
plot( p4, col=rgb(1,0,0,1/4), main = "Challengingness score distribution", xlab = "Score")  # first histogram
plot( p3, col=rgb(0,0,1,1/4), add=T)  # second
legend(4,5,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))

p5 <- hist(eyeQ$How.much.fun.was.the.task.[eyeQ$How.much.fun.was.the.task.!=0 & eyeQ$Typing.mechanism=='DT'], main = "How much fun was the task?", xlab = "Score", xlim = c(1,10), breaks = c(0,1,2,3,4,5,6,7,8,9,10))
p6 <- hist(eyeQ$How.much.fun.was.the.task.[eyeQ$How.much.fun.was.the.task.!=0 & eyeQ$Typing.mechanism=='MS'], main = "How much fun was the task?", xlab = "Score", xlim = c(1,10), breaks = c(0,1,2,3,4,5,6,7,8,9,10))
plot( p5, col=rgb(0,0,1,1/4), xlim=c(1,10), main = "Fun score distribution", xlab = "Score")  # first histogram
plot( p6, col=rgb(1,0,0,1/4), xlim=c(1,10), add=T)  # second
legend(4,5,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))


# Histogram on typing speed in wpm
p7 <- hist(eyeQ$Typing.Speed[eyeQ$Typing.mechanism=="DT"])
p8 <- hist(eyeQ$Typing.Speed[eyeQ$Typing.mechanism=="MS"])

plot( p7, col=rgb(0,0,1,1/4), xlim=c(0,14), main = "Typing speed distribution", xlab = "Typing speed [in wpm]")  # first histogram
plot( p8, col=rgb(1,0,0,1/4), xlim=c(0,14), add=T)  # second
legend(11,7,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))

# Check the significance of the difference : Non-parametric (MS is not normal), independent samples (different populations performed different typing mechanisms) Kruskal test as variances are not equal for both gropups
kruskal.test(eyeQ$Typing.Speed ~ eyeQ$Typing.mechanism, data = eyeQ)

# Box plot to visualise typing speed
boxplot(eyeQ$Typing.Speed , eyeQ$Typing.mechanism, data = eyeQ, main='Typing mechanism and speed', xlab = 'Typing mechanism', ylab = 'Typing speed')

# Histogram on error rate
MSDwoZero <- subset(eyeQ$MSD.error.rate, eyeQ$MSD.error.rate!=0.00)
eyeTyping <- subset(eyeQ$Typing.mechanism, eyeQ$MSD.error.rate!=0.00)
p9 <- hist(eyeQ$MSD.error.rate[eyeQ$Typing.mechanism=="DT"])
p10 <- hist(eyeQ$MSD.error.rate[eyeQ$Typing.mechanism=="MS"])

plot( p9, col=rgb(0,0,1,1/4), xlim=c(0,100), ylim =c(0,10), main = "Error rate distribution", xlab = "Error rate")  # first histogram
plot( p10, col=rgb(1,0,0,1/4), xlim=c(0,100), ylim =c(0,10), add=T)  # second
legend(80,10,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))

kruskal.test(MSDwoZero ~ eyeTyping)

boxplot(MSDwoZero ~ eyeTyping, data = eyeQ, main='Typing mechanism and error', xlab = 'Typing mechanism', ylab = 'Error [in %]')

# Histogram on Gaze To Text field per Char typed
p11 <- hist(eyeQ$GazeToTextFieldFrequencyPerCharacter[eyeQ$Typing.mechanism=='DT'], breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2))
p12 <- hist(eyeQ$GazeToTextFieldFrequencyPerCharacter[eyeQ$Typing.mechanism=='MS'], breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2))
plot( p11, col=rgb(0,0,1,1/4), main = "Gaze to text field per typed characters distribution", xlab = "Frequency", ylim = c(0,9), xlim = c(0,1.2))  # first histogram
plot( p12, col=rgb(1,0,0,1/4), add=T)  # second
legend(1,8,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))

kruskal.test(eyeQ$GazeToTextFieldFrequencyPerCharacter ~ eyeQ$Typing.mechanism)

boxplot(eyeQ$GazeToTextFieldFrequencyPerCharacter ~ eyeQ$Typing.mechanism, data = eyeQ, main='Typing mechanism and gazes to text field', xlab = 'Typing mechanism', ylab = 'Gaze to Text field per Character typed')

# Histogram on Attended but not selected char per char typed
p13 <- hist(eyeQ$AttendedButNotSelected[eyeQ$Typing.mechanism == 'DT'], breaks= c(0.5,1,1.5,2,2.5,3))
p14 <- hist(eyeQ$AttendedButNotSelected[eyeQ$Typing.mechanism == 'MS'], breaks= c(0.5,1,1.5,2,2.5,3))
plot( p13, col=rgb(0,0,1,1/4), main = "Attended but not selected characters per characters typed", xlab = "Ratio of characters attended but not selected to number of characters typed", ylim = c(0,9), xlim = c(0,3))  # first histogram
plot( p14, col=rgb(1,0,0,1/4), add=T, ylim = c(0,9), xlim = c(0,3))  # second
legend(2.5,8,c("Dwell-time", "Multi-swipe"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))

kruskal.test(eyeQ$AttendedButNotSelected ~ eyeQ$Typing.mechanism)

boxplot(eyeQ$AttendedButNotSelected ~ eyeQ$Typing.mechanism, data = eyeQ, main = 'Typing mechanism and attended but not selected characters', xlab = 'Typing mechanism', ylab = 'Attended but not selected characters per characters typed')
