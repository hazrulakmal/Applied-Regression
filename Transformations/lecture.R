# Lecture
library(car)
library(ggplot2)
library(gridExtra)
library(arm)


hourly_pay <- read.csv("data/age_hourpay.csv", header = TRUE, stringsAsFactors = TRUE)

#create a new variable called genderCat where gender is M,F
hourly_pay$genderCat<-ifelse(hourly_pay$gender==0,"M","F")
#0 is male

summary(hourly_pay)

lm.1 <- lm(hourpay ~ age * factor(genderCat), hourly_pay)
display(lm.1)

#if you want to study male. intercept = 8.41-1.56 age = 0.08+0.12
#The intercept says that a male of age 0 earns 8.41 an hour (weird intrepreataion)

# centering the age (manipulating the values in a column)
hourly_pay$cent.age <- with(hourly_pay, age-mean(age))
lm.2 <- lm(hourpay ~ cent.age * factor(genderCat), hourly_pay)
display(lm.2)

mean(hourly_pay$age)
#The coefficient associated with age does not change, however we can now see that for a person who is 41.7 years old (mean of age) men earn on average 3.46 pounds more than women.
# intrepretation on the intercept - women at the age  41.7 years old earn on average 11.88ph

#we can argue that a predictor is more important than the other by looking at the maginitude of the coeeficient. like in this case, we may say age is less important than gender because 0.08<<3.46 but this 
#is not always the case, the range of age is 1-67 while gender is 1-0, so to compare between predictors we may want to standardise them. 
#log transform cannot be aplied to variables with 0 and negative (log properties) - one way to deal with this is by adding/substracing a constant to make all variables positive integers. then inverse the equation later. 

hourly_pay$std.age <- with(hourly_pay, (age-mean(age))/sd(age))
hourly_pay$std.gender <- with(hourly_pay, (gender-mean(gender))/sd(gender))

std.gender.2<-with(hourly_pay,(gender-mean(gender)/sd(gender)))

lm.sd.1 <- lm(hourpay ~ std.age + std.gender, hourly_pay)              
display(lm.sd.1)
#interesting the coefficient is the same. 

lm.sd.2 <- lm(hourpay ~ std.age + std.gender.2, hourly_pay)              
display(lm.sd.2)
#A change in 2 years of (2× 1.73=3.46) has approximately the same effect on hourly pay as being a man vs being a woman.

# Log Transformation (go-to statistician transformation)
# to tackle linearity - subset analysis, splitting the the data according to the linearity relationships 
#log transformation should be considered for some reasons
# when the outcome is negative, the residual plot exhibits funnel shape, theoretical reasons(economics and statistics . 

# in case of age as x-axis and salary as y-axis
# funnel shapes mean that as as the age increases, the salary variations increases as well so non-constant variance.

# problem with funnel shape residual plots
# hypothesis test relies on variance being constant

lm.0 <- lm(hourpay ~ age + genderCat, hourly_pay)

pred.for.plot<-fitted(lm.0)
#plot of raw data
p1<-ggplot(data=hourly_pay, aes(x=age, y=hourpay, colour=genderCat, shape=genderCat))+ geom_point(size = 0.1)

#using geom_line to fit a line through the fitted points (i.e. the expected line)
p1<-p1+geom_line(aes(y =pred.for.plot))
p1

p2<-ggplot(data=hourly_pay,aes(x=pred.for.plot,y=rstandard(lm.0), colour=genderCat, shape=genderCat))+geom_point(size = 0.1)+geom_abline(slope=0,intercept=0)
p2

#cut the extreme salary values
p3<-ggplot(data=hourly_pay, aes(x=age, y=hourpay, colour=genderCat, shape=genderCat))+ geom_point(size = 0.1)+geom_line(aes(y =pred.for.plot), size = 1)
#only points between -1 and 150 in hourpay should be displayed
p3<-p3 + ylim(-1,150)
p4<-ggplot(data=hourly_pay,aes(x=pred.for.plot,y=rstandard(lm.0), colour=genderCat, shape=genderCat))+geom_point(size = 0.15)+geom_abline(slope=0,intercept=0)
p4<-p4+ylim(-5,10)
p4 <- p4 + theme(legend.position="none",text = element_text(size=rel(3)))+ xlab("fitted")+ylab("standardised residuals")
grid.arrange(p3,p4 ,ncol=1)


#log the salary

lm.log.1 <- lm(log(hourpay) ~ age + gender, hourly_pay)

pred.for.plot.2 <- fitted(lm.log.1)
p5 <- ggplot(data=hourly_pay, aes(x=age, y=log(hourpay), colour=genderCat, shape=genderCat))+ geom_point(size = 0.1)+geom_line(aes(y =pred.for.plot.2), size = 1)
#only points between -1 and 150 in hourpay should be displayed
  
p6 <- ggplot(data=hourly_pay,aes(x=pred.for.plot.2,y=rstandard(lm.log.1), colour=genderCat, shape=genderCat))+geom_point(size = 0.25)+geom_abline(slope=0,intercept=0)
p6  
  
  
  
  
  
  
  
  
  