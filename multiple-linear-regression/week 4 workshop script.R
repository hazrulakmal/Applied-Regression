library(arm)
library(ggplot2)
library(tidyr)


Study_habits<-read.csv("Stats_study_habits_mlr.csv")
head(Study_habits, 5)

grade.lm.1 <- lm(Grade~Study.Skills+Enjoy, data=Study_habits)
display(grade.lm.1)
coef(grade.lm.1)

#compare the residual sd with the range of the data. 

summary(grade.lm.1)

p1<-ggplot(Study_habits, aes(x=Study.Skills, y=Grade,colour=factor(Enjoy))) + geom_point(aes(shape=factor(Enjoy)))
p1
p1<- p1+ geom_abline(intercept = coef(grade.lm.1)[1], slope = coef(grade.lm.1)[2], color="red")
p1<- p1 + geom_abline(intercept = (coef(grade.lm.1)[1]+ coef(grade.lm.1)[3]),slope = coef(grade.lm.1)[2], color = "blue")
p1<- p1 +scale_colour_grey(start = 0.1, end = 0.6)  + theme_bw()


# bookstrap 
Study_habits.Enj1<-subset(Study_habits, Enjoy==1)
lm.Enj1<-lm(Grade~Study.Skills,data=Study_habits.Enj1)
Study_habits.Enj0<-subset(Study_habits, Enjoy==0)
lm.Enj0<-lm(Grade~Study.Skills,data=Study_habits.Enj0)

p1<-ggplot(Study_habits, aes(x=Study.Skills, y=Grade,colour=factor(Enjoy)))
p1<- p1 + geom_point(aes(shape=factor(Enjoy)))
#Plot a line based on the lm.Int0 for the subset of people with Enjoy=0
p1<- p1 + geom_abline(intercept=coef(lm.Enj0)[1], slope=coef(lm.Enj0)[2], color="black")
#Plot a line based on the lm.Int1 for the subset of people with Enjoy=1
p1<- p1 + geom_abline(intercept=coef(lm.Enj1)[1], slope=coef(lm.Enj1)[2],color="gray")
#for black and white. you can ignore
p1<- p1 +scale_colour_grey(start = 0.1, end = 0.6)  + theme_bw()
p1

#good reason to add interactions, 1. strong predictor 2. expert says there's an interaction.

grade.enjoy.lm.int<-lm(Grade~Study.Skills*Enjoy,data=Study_habits)
display(grade.enjoy.lm.int)


p1<-ggplot(Study_habits, aes(x=Study.Skills, y=Grade,colour=factor(Enjoy)))
#add points
p1<- p1 + geom_point(aes(shape=factor(Enjoy)))
#In ggplot it is much easier to plot the interaction!
# se=FALSE doesn't add the standard error range to the regression. Try without it!
p1<- p1+geom_smooth(method="lm", se=FALSE) 
#black and white colour scheme, you can ignore
p1<- p1 +scale_colour_grey(start = 0.1, end = 0.6)  + theme_bw()
p1


# F-statistics just looks at the model as  a whole. not looking at which predictors are better than the other. 

grade.lm<-lm(Grade~Study.Skills+Interesting+Enjoy,data=Study_habits)
display(grade.lm)
summary(grade.lm)
