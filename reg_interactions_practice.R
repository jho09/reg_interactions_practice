library(tidyverse)
my_data <- read.table(file="lectureData6060.csv",header=TRUE,sep=",",na.strings=c("NA"))
glimpse(my_data)

analytic.data <- my_data %>% select(Exam,Anxiety,Preparation)
#keep complete cases only, list-wise deletion of cases
analytic.data <- na.omit(analytic.data)

# add column with mean center anxiety
analytic.data <- analytic.data %>%
  mutate(x.centered=as.numeric(scale(Anxiety,center=T,scale=F)) )
# add column with mean center preparation
analytic.data <- analytic.data %>%
  mutate(z.centered=as.numeric(scale(Preparation,center=T,scale=F)) )


interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered),
                             data=analytic.data, na.action=na.exclude)
summary(interaction.regression)

library(apaTables)
apa.reg.table(interaction.regression)


block1 <- lm(Exam ~ x.centered + z.centered,
             data=analytic.data, na.action=na.exclude)
block2 <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered),
             data=analytic.data, na.action=na.exclude)

apa.reg.table(block1, block2, filename="myRegressionTable1.doc",table.number=1)


sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data <- analytic.data %>%
  mutate(z.centered.at.plus.1SD = z.centered - sd.z)
#This may seem counter intuitive, but we lower the scores to increase the zero point to +1 SD
simple.slope.plus.1SD <- lm(Exam ~ x.centered + z.centered.at.plus.1SD
                            + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude)
summary(simple.slope.plus.1SD)

analytic.data <- analytic.data %>%
  mutate(z.centered.at.minus.1SD=z.centered + sd.z)
simple.slope.minus.1SD <- lm(Exam ~ x.centered + z.centered.at.minus.1SD
                             + I(x.centered*z.centered.at.minus.1SD),
                             data=analytic.data,na.action=na.exclude)
summary(simple.slope.minus.1SD)

sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)
#we want the x-axis of the graph to range from -2 SD to +2 SD
x.axis.range <-seq(-2*sd.x,2*sd.x,by=.25*sd.x)

sd.z<-sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi= 1*sd.z
z.line.lo= -1*sd.z

#+1SD Line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.hi)
#-1SD Line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.lo)
#Put the information describing the lines into a data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)


library(ggplot2)
#set default (x,y) variables
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))
#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5)
#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus.1SD.z),
                               color="black",linetype="solid",size=1.5)
#set APA part of graph below

my.plot <- my.plot + theme_classic()
print(my.plot)


my.plot <- my.plot+annotate("text", x = -1, y = 68.5, label = "+1 SD Preparation")
my.plot <- my.plot+annotate("text", x = -1, y =43.5, label = "-1 SD Preparation")


#Sometimes you want to look for other product terms. You would run the regression with those terms.
interaction.regression <- lm(Exam ~ x.centered + z.centered
                             + I(x.centered*z.centered)
                             + I(x.centered*x.centered) + I(z.centered*z.centered),
                             data=analytic.data,na.action=na.exclude)
summary(interaction.regression)

sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)
#we want the x-axis of the graph to range from -2 SD to +2 SD
x.axis.range <-seq(-2*sd.x,2*sd.x,by=.25*sd.x)
sd.z<-sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi= 1*sd.z
z.line.lo= -1*sd.z

#+1SD Line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.hi)
#-1SD Line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.lo)
#Put the information describing the lines into a data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)


library(ggplot2)
#set default (x,y) variables
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))
#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5)
#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus.1SD.z),
                               color="black",linetype="solid",size=1.5)
#set APA part of graph below
my.plot <- my.plot + theme_classic()
print(my.plot)

interaction.regression <- lm(Exam ~ x.centered + z.centered
                             + I(x.centered*z.centered),
                             data=analytic.data,na.action=na.exclude)

summary(interaction.regression)


library(MBESS)
intr.plot(b.0=55.5794,b.x=-2.1503,b.z=4.5648,b.xz=0.9077,
          x.min=-2*sd.x,x.max=2*sd.x,z.min=-2*sd.z,z.max=2*sd.z)