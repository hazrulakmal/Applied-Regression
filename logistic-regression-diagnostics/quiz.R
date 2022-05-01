#Q4 exp(0.49) type #Q5 2*invlogit
library(car)
library(arm)
library(ggplot2)
library(gridExtra)

gun.dt <- read.csv("src/gunUSA.csv", header = TRUE, stringsAsFactors = TRUE)
summary(gun.dt)
colnames(gun.dt)

p1 <- with(gun.dt, plot(MedianHHInc,Crime))
p2 <- with(gun.dt, boxplot(MedianHHInc~Open,xlab="Guns allowed"))
p3 <- with(gun.dt, boxplot(Crime~Open,xlab="Guns allowed"))
p4 <- with(gun.dt, boxplot(Population~Open,xlab="Guns allowed", ylab="Pop/Mill"))
p5 <- with(gun.dt, plot(Population,Crime))
p6 <- with(gun.dt, plot(Population,MedianHHInc))


with(gun.dt, table(Open,President))
with(gun.dt, mosaicplot(table(President,Open)))
with(gun.dt, mosaicplot(table(RighttoWork,Open))) #no association. boxes are identical to one another
with(gun.dt, mosaicplot(table(Region,Open))) #association exits

glm.first <- glm(Open ~ Population+MedianHHInc+President+Region, data= gun.dt, family=binomial(link="logit"))
display(glm.first)

#Average Predictive Comparision 
gun.glm.apc<-glm(Open~Population+MedianHHInc,data=gun.dt, family=binomial(link='logit'))
display((gun.glm.apc))
with(gun.dt,range(MedianHHInc))

#this returns a max of around 7.4 and a low of 3.9
b<-coef(gun.glm.apc)
hi<-7.4
lo<-3.9

delta.med<-with(gun.dt,(invlogit(b[1]+b[2]*Population+b[3]*hi)-invlogit(invlogit(b[1]+b[2]*Population+b[3]*lo))))
mean(delta.med)

with(gun.dt,range(Population))
high <- 38.3
low <- 0.58

delta.pop<-with(gun.dt,(invlogit(b[1]+b[2]*hi+b[3]*MedianHHInc)-invlogit(b[1]+b[2]*low+b[3]*MedianHHInc)))
mean(delta.pop)

anova(gun.glm.apc, glm.first, test = "Chisq" )

invlogit(coef(gun.glm.apc)[3])

