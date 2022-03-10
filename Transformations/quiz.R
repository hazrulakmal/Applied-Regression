# Quiz 6

library(arm)
library(ggplot2)
library(tidyr)
library(car)
library(gridExtra)

youth.employment <- read.csv("data/YNEET.csv", header = TRUE, stringsAsFactors = TRUE)
summary(youth.employment)

#Function
centerit.fun<-function(v){v-mean(v)}
standardiseit.fun<-function(v){(v-mean(v))/(2*sd(v))}

#fit non-centre model
lm.1 <- lm(YNEET ~ SpendStu + GINI, youth.employment)
display(lm.1)

cen.SpendStu <- centerit.fun(youth.employment$SpendStu)
cen.gini <- centerit.fun(youth.employment$GINI)

lm.2 <- lm(YNEET ~ cen.gini + cen.SpendStu, youth.employment)
display(lm.2)

mean(youth.employment$SpendStu)
mean(youth.employment$GINI)

#Identifying predictor with large effect - look at significant p value

std.SpendStu <- standardiseit.fun(youth.employment$SpendStu)
std.gini <- standardiseit.fun(youth.employment$GINI)

std.lm.1 <- lm(YNEET ~ std.SpendStu+ std.gini, youth.employment)
display(std.lm.1)

#-----------------------------------------------
  
population <- read.csv("data/Population.csv", header = TRUE, stringsAsFactors = TRUE)
summary(population)

p1 <- ggplot(data=population, aes(x=Year, y=Population, colour=Country))+geom_line()
p1

#Transformation
population.sqrt <- sqrt(population$Population)
population.inverse <- 1/population$Population
population.log <- log(population$Population)

p2 <- ggplot(data=population, aes(x=Year, y=population.sqrt, colour=Country))+geom_line()
p3 <- ggplot(data=population, aes(x=Year, y=population.inverse, colour=Country))+geom_line()
p4 <- ggplot(data=population, aes(x=Year, y=population.log, colour=Country))+geom_line()
grid.arrange(p2,p3,p4 , nrow=3)

#Regression model comparison
pop.lm <- lm(Population ~ Country+Year, population)
display(pop.lm)
pop.log.lm <- lm(log(Population) ~., population)
display(pop.log.lm)

#------------------------------------------------

harry_potter <- read.csv("data/harrypotter.csv", header = TRUE, stringsAsFactors = TRUE)
summary(harry_potter)

harry_potter$revenue <- harry_potter$revenue/100000
with(harry_potter,aggregate(harry_potter[,7],by=list(weeknum=weeknum),mean))

with(harry_potter,aggregate(harry_potter[,7],by=list(film=film),mean))

p5 <- ggplot(harry_potter, aes(x=weeknum, y=revenue, colour=factor(film)))+geom_point()
#revenue shows decaying trend (inverse function) as week passes by
p6 <- ggplot(harry_potter, aes(x=weeknum, y=log(revenue), colour=factor(film)))+geom_point()
grid.arrange(p5, p6, nrow=2)

#-------------------------------------------------

harr.lm <- lm(log(revenue) ~ weeknum*film, harry_potter )
display(harr.lm)

#The output of display indicates that the model has a good fit. The diagnostics are generally good (residual sd is low w.r.t to the range of log(revenue), the R2 while not completely relevant here is very high) and all the interactions are significant. 
p7 <- ggplot(data = harry_potter, aes(x = weeknum, y = log(revenue), colour=factor(film))) + geom_point()
p7 <- p7+ facet_wrap(~factor(film))+geom_smooth(method="lm",se=FALSE)
p7
plot(harry_potter, which=c(1))



















