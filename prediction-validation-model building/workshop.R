#Workshop 8

library(arm)
library(ggplot2)
library(tidyr)
library(car)
library(gridExtra)

Hourly_Pay<-read.csv("data/Large_Hourly_Pay.csv",header = TRUE, stringsAsFactors = TRUE)
Hourly_Pay_NoRich<-subset(Hourly_Pay,hourpay<=150) #reudce the variability of the data
summary(Hourly_Pay_NoRich)

#set baseline levels
Hourly_Pay_NoRich$OwnHouse<-relevel(Hourly_Pay_NoRich$OwnHouse,ref="Own")
Hourly_Pay_NoRich$Ethnicity<-relevel(Hourly_Pay_NoRich$Ethnicity,ref="White")
Hourly_Pay_NoRich$HighestQual<-relevel(Hourly_Pay_NoRich$HighestQual,ref="DegreeHE")
Hourly_Pay_NoRich$gender<-ifelse(Hourly_Pay_NoRich$gender==0,"M","F")

#base level for which you are interested in comparing. example likt those codes up above. 

#Using facet_wrap to display the scatterplots
#line.1
colnames(Hourly_Pay_NoRich)
special.dat<-gather(Hourly_Pay_NoRich[,c(3,4,6,7),] ,-hourpay, key="var", value="value") 

#line.2
p1<-ggplot(special.dat,aes(x = value, y = log(hourpay)))+geom_point(size=0.5) +geom_smooth(method="lm", se=FALSE, color="black") +facet_wrap(~ var, scales = "free_x")
# scales = "free_x" let the x-axis range be chosen by the computer
p1

# Define the categorical variables as well as hourpay
special.dat<- gather(Hourly_Pay_NoRich[,c(2,5,6,8)],key="variable", value="value", -hourpay)  
#plots the boxplots using "free_x"
p2<-ggplot(special.dat, aes(x=factor(value), y = log(hourpay), fill=factor(value))) +geom_boxplot() +facet_wrap(~ variable, scales = "free_x")
#for book
p2

grid.arrange(p1,p2, nrow=2)

#Fit the model
all.lm<-lm(hourpay~.,data=Hourly_Pay) # if we want to exclude the variable just add -predictor 
#shortcut if you want to use all the predictors in the dataset
all.lm<-lm(hourpay~age+TotalHoursWorked+AgeYoungDep+gender 
           +OwnHouse+HighestQual+Ethnicity,data=Hourly_Pay_NoRich)
display(all.lm, digits=3)

#standardize function
standardise.fn<-function(v){(v-mean(v))/(sd(v))}

std.age<-standardise.fn(Hourly_Pay_NoRich$age)
std.THW<-standardise.fn(Hourly_Pay_NoRich$TotalHoursWorked)
std.AYD<-standardise.fn(Hourly_Pay_NoRich$AgeYoungDep)

std.all.lm<-lm(hourpay~std.age+std.THW+std.AYD+gender
               +OwnHouse+HighestQual+Ethnicity,data=Hourly_Pay_NoRich)
display(std.all.lm, digits=3)
#we can decide which predictors influence the prediction the most by looking at the abs value.

Anova(all.lm)

#residual plots
par(mfrow=c(1,3))
plot(all.lm,which=c(1,2),cex=0.7,pch=".")
hist(all.lm$residuals,main="Histogram of standardised residuals", xlab="Std. residuals")


log.lm<-lm(log(hourpay)~.,data=Hourly_Pay_NoRich)
par(mfrow=c(1,3))
plot(log.lm,which=c(1,2),cex=0.7,pch=".")
hist(log.lm$residuals,main="Histogram of standardised residuals", xlab="Std. residuals")

display(log.lm) 

#to combact non-linearity problem. add a polynomial higher order term
age2<-Hourly_Pay_NoRich$age*Hourly_Pay_NoRich$age
quad.age.lm<-lm(log(hourpay)~age+age2+TotalHoursWorked+AgeYoungDep+gender 
                +OwnHouse+HighestQual+Ethnicity,data=Hourly_Pay_NoRich)
display(quad.age.lm, digits=3)
# better fit (r-squaared increased) but not significantly. at this point, it's difficult to improve the prediction much more
pred.for.plot<-fitted(quad.age.lm)
#plot of raw data
p3<-ggplot(data=Hourly_Pay_NoRich, aes(x=age, y=log(hourpay)))
#using geom_line to fit a line through the fitted points (i.e. the expected line)
p3<-p3+geom_smooth(method="lm", formula=y~poly(x,2), size = 1)
#not essential, for the book
p3<-p3+geom_point( size=0.5)
p3

par(mfrow=c(1,3))
plot(quad.age.lm,which=c(1,2),cex=0.7,pch=".")
hist(quad.age.lm$residuals,main="Histogram of standardised residuals", xlab="Std. residuals")

log.age.lm<-lm(log(hourpay)~log(age)+TotalHoursWorked+AgeYoungDep+gender 
               +OwnHouse+HighestQual+Ethnicity,data=Hourly_Pay_NoRich)

display(log.age.lm, digits=3)
#Residual plots 
par(mfrow=c(1,3))
plot(log.age.lm,which=c(1,2),cex=0.7,pch=".")
hist(log.age.lm$residuals,main="Histogram of standardised residuals", xlab="Std. residuals")








