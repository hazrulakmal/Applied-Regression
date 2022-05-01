library(arm)
library(car)

golf.dt <- read.csv("src/golf.full.csv", header = TRUE)
head(golf.dt, 5)
colnames(golf.dt)

#Apply logistics regression
golf.glm<-glm(Success~Feet, data=golf.dt, family=binomial(link="logit"))
display(golf.glm)
#The further the feet, the lower the probability of sucess.

icu.dat<-read.csv("src/ICUdeaths.csv", header=TRUE)
colnames(icu.dat)

icu.glm.1<-glm(Lived~Age,dat=icu.dat, family=binomial(link="logit"))
display(icu.glm.1)
#Predictors are statistcally significant at 5% level. 

icu.glm.2<-glm(Lived~Age+SBP,dat=icu.dat, family=binomial)
display(icu.glm.2, digits=4)
#second model with additional predictors (SBP)
#Qustion we are interested in now is how much better is the the model with SBP as a predictor than the one without it.
#Looking at the difference of residual deviance between these two model and check if the difference is statiscally significant at 5% level by using chi-square with k degree of fredom.
#k is the number of additional estimated parameters added into a model. 

icu.glm.3<-glm(Lived~Age+Sex,dat=icu.dat, family=binomial)
display(icu.glm.3)

#compares model with Age and that with Age+Sex
anova(icu.glm.1, icu.glm.3, test="Chisq") # non-significant.  model 3 is no better than model 1. adding sex doesnt improve the model. 

icu.glm.4<-glm(Lived~Age+Sex+SBP+HR,dat=icu.dat, family=binomial)
display(icu.glm.4)

anova(icu.glm.2, icu.glm.4, test="Chisq")




