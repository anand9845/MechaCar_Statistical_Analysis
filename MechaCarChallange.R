library(readr)
##################Question1############################
##~~~~~~~~~~~~~~~~~~~~~~~~~~Delievrable1~~~~~~~~~~~~~~~~~~~~~~~~

data1  <- read.csv("C:\Users\bajaj\UOT\Assignments\R Machine\MechaCar_mpg.csv", header=TRUE, stringsAsFactors=FALSE)
head(data1)
~~~~~~~~~~~~~~~~~~~~~~~~Delevriable2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages('caTools')
library(caTools)
split = sample.split(data1$mpg, SplitRatio = 0.7)
trainingset = subset(data1, split == TRUE)
testset = subset(data1, split == FALSE)
data2<-data1
mean(data1$mpg)
data2$mpg<-NULL
# Fitting Simple Linear Regression to the Training set
lm.r= lm(formula = mpg~vehicle_weight+spoiler_angle+vehicle_length+spoiler_angle+ground_clearance+AWD,data = trainingset)
coef(lm.r)

# Predicting the Test set results
ypred = predict(lm.r, newdata = testset)
mean(ypred)
ypred
###########################Delievrable3###################
ym<- mean(ypred)
t.test(rnorm(ym))
  ##################Graph###########################

install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x = testset$YearsExperience, y = testset$Salary),
             colour = 'red') +
  geom_line(aes(x = trainingset$YearsExperience,
                y = predict(lm.r, newdata = trainingset)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

ggplot() +
  geom_point(aes(x = testset$vehicle_length+testset$vehicle_weight+testset$spoiler_angle+testset$ground_clearance+testset$AWD, y = testset$AWD),
             colour = 'red') +
  geom_line(aes(x = trainingset$vehicle_length+trainingset$vehicle_weight+trainingset$spoiler_angle+trainingset$ground_clearance+trainingset$AWD, y = trainingset$AWD,
                y = predict(lm.r, newdata = trainingset)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')
sapply(data1,var)
var(data1$vehicle_length)
var(data1$vehicle_weight)
var(data1$spoiler_angle)
var(data1$ground_clearance)
var(data1$AWD)
summary(data1$mpg)
summary(ypred)
 ##################Task2############################

###~~~~Delevriable1
library(readr)
data2  <- read.csv("C:/Users/M.Abdullah Khilji/Documents/Project2/Suspension_Coil.csv", header=TRUE, stringsAsFactors=FALSE)
head(data2)

####~~~Delevriable2~~~~~~~~~~~~~~~
MEAN<-tx1<-mean(data2$PSI)
MEDIAN<-median(data2$PSI)
VARIANCE<-var(data2$PSI)
STANDARDDEVIATION<-sd(data2$PSI)
df2<-data.frame(MEAN,MEDIAN,VARIANCE,STANDARDDEVIATION)


total_summary<-df2
install.packages("dplyr")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###############Deleivrable3#################
lot1<-df[df$Manufacturing_Lot == 'Lot1', ]
lot2<-df[df$Manufacturing_Lot == 'Lot2', ]
lot3<-df[df$Manufacturing_Lot == 'Lot3', ]

tl1<-mean(lot1$PSI)
median(lot1$PSI)
var(lot1$PSI)
sd(lot1$PSI)


tl2<-mean(lot2$PSI)
median(lot2$PSI)
var(lot2$PSI)
sd(lot2$PSI)



tl3<-mean(lot3$PSI)
median(lot3$PSI)
var(lot3$PSI)
sd(lot3$PSI)

MEAN<-c(mean(lot1$PSI),mean(lot2$PSI),mean(lot3$PSI))
MEDIAN<-c(median(lot1$PSI),median(lot2$PSI), median(lot3$PSI))
VARIANCE<-c(var(lot1$PSI),var(lot2$PSI), var(lot3$PSI))
STANDARDDEVIATION<-c(sd(lot1$PSI),sd(lot2$PSI), sd(lot3$PSI))
df3<-data.frame(MEAN,MEDIAN,VARIANCE,STANDARDDEVIATION)

#########################################Task3###################################


##~~~~~~~~~~~~~~~~Delevirable1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
t.test(rnorm(mean(data2$PSI)), rnorm(tx1))
##~~~~~~~~~~~~~~~~Delevirable2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
t.test(rnorm(tx1), rnorm(tl1))
t.test(rnorm(tx1), rnorm(tl2))
t.test(rnorm(tx1), rnorm(tl3))



###########################Task4#################################
###############T test Delevirable #####################
t.test(rnorm(mean(data1$mpg)))





