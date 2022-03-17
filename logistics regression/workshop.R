library(arm)
library(ggplot2)
library(gridExtra)
library(car)
library(dplyr)

icu.dat<-read.csv("ICUdeaths.csv", header=TRUE, stringsAsFactors = TRUE)
head(icu.dat, 5)
summary(icu.dat)

#EDA
p1<- ggplot(icu.dat, aes(x=factor(Lived), y=Age))+ geom_boxplot()+ coord_flip()#horizontal boxplot
p1<-p1+theme(legend.position = "none")

p2<- ggplot(icu.dat, aes(x=factor(Lived), y=SBP))+ geom_boxplot()+ coord_flip()
p2<-p2+theme(legend.position = "none")

p3<- ggplot(icu.dat, aes(x=factor(Lived), y=HR))+ geom_boxplot()+ coord_flip()
p3<-p3+theme(legend.position = "none")

#cross tabulation plot: are two categorical variables related?
p4<- ggplot(icu.dat,aes(x = factor(Sex), fill = factor(Lived))) +
  geom_bar(position = "fill") +  scale_y_continuous(name = "Within group Percentage"
                                                    , labels = scales::percent)

grid.arrange(p1,p2,p3,p4,nrow=2)

#GLM
icu.glm<-glm(Lived~Age,data=icu.dat,family=binomial(link="logit"))
display(icu.glm)
summary(icu.glm)

#coeffcit
#odds ratio assoc with an increase in 1 year. exp(-0.04) = 0.94
invlogit(3.15-0.04*20) #0.913
invlogit(3.15-0.04*50) #0.760
0.91/0.76 #1.2 


icu.breaks<-mutate(icu.dat, bin=cut(Age,breaks = seq(15, 95, by=5)))

#estimate the proportion of people that survived the ICU in each bin (y-axis)
prop <- prop.table(with(icu.breaks, table(Lived,bin)),2)[2,]
# the midpoint of each bin (x-axis)
midbin<- seq(17.5, 92.5, by=5)
#create a new data frame using these two variables
icu.bin<-data.frame(midbin,prop)
#icu.bin

## ----the logistic plot----
#basic plot 
p1<- ggplot(icu.breaks,aes(x=Age,y=Lived))
# geom_count is a type of point that takes into account how frequent that value is
# it plots the Lived/not Lived 
p1<-p1+ geom_count() 
#adds the logisitc regression line of best fit 
p1<-p1+ geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)
#plots the points corresponding to the proportions
p1<-p1+ geom_point(data=icu.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3) 

p1

icu.dat.shape
((19+152)/(96+141))

## ---------centering---------------------------------------------------------------
cent.Age<-with(icu.dat, (Age-mean(Age)))
cent.age.glm<-glm(Lived~cent.Age, data=icu.dat, family = binomial(link="logit"))
display(cent.age.glm)

## ------------------predicting------------------------------------------------------
pred.glm<-as.numeric(icu.glm$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm, observed=icu.dat$Lived)
table(glm.dat)

## ----------------------table of percentages correct--------------------------------------------------
round(prop.table(with(icu.dat, table(Lived,Sex)),2),2)

## -----------------------fake data-------------------------------------------------
#fake data with an association: 
#A binary with 30 1's and 50 0's.
A<-c(rep(1,30),rep(0,50))
#rbinom generates n binary random variables with p(x=1)=p. 
#size picks a sample of size from these
B<-ifelse(A==1,rbinom(n=100,size=1,p=0.2),rbinom(n=100,size=1,p=0.8))
#show A,B
A
B
#There is a strong dependence between A and B
AB<-data.frame(A=A, B=B)
round(prop.table(table(A,B),2),2)

## ----plots to compare-------------
p1<- ggplot(icu.dat,aes(x = factor(Sex), fill = factor(Lived))) +
  geom_bar(position = "fill") +  scale_y_continuous(name = "Within group Percentage"
                                                    , labels = scales::percent)
p1<-p1+ labs(tag = "A")

p2<- ggplot(data=data.frame(A,B),aes(x = factor(A), fill = factor(B))) +
  geom_bar(position = "fill") +  scale_y_continuous(name = "Within group Percentage"
                                                    , labels = scales::percent)
p2<-p2+ labs(tag = "B")
grid.arrange(p1,p2,nrow=1)






