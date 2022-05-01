library(car)
library(ggplot2)
library(arm)
library(gridExtra)

#load the data
nursery_dt <- read.table("NurserySchools.txt", header = TRUE)
head(nursery_dt, 5)

p1 <- ggplot(data = nursery_dt, aes(x=Dep, y= NurseryExp)) + geom_point() + geom_smooth(method = "lm")
p2 <- ggplot(data = nursery_dt, aes(x=NurseryProv , y= NurseryExp)) + geom_point() + geom_smooth(method = "lm")
grid.arrange(p1,p2,nrow=1)

with(nursery_dt, cor(Dep, NurseryExp))
with(nursery_dt, cor(NurseryProv,NurseryExp))

#Q2 remove outlies (max)
out1 <- which(nursery_dt$NurseryExp==max(nursery_dt$NurseryExp))
first_lm <- lm(NurseryExp ~ Dep + NurseryProv , data = nursery_dt)
display(first_lm)

nursery_out.1 <- nursery_dt[-out1,]
imp_first <- lm(NurseryExp ~ Dep + NurseryProv , data = nursery_out.1)
display(imp_first)

(15.85-14.81)/15.85

#Q3
show_outliers<-function(the.linear.model,topN){
  #length of data
  n=length(fitted(the.linear.model))
  #number of parameters estimated
  p=length(coef(the.linear.model))
  #standardised residuals over 3
  res.out<-which(abs(rstandard(the.linear.model))>3) #sometimes >2
  #topN values
  res.top<-head(rev(sort(abs(rstandard(the.linear.model)))),topN)
  #high leverage values
  lev.out<-which(lm.influence(the.linear.model)$hat>2*p/n)
  #topN values
  lev.top<-head(rev(sort(lm.influence(the.linear.model)$hat)),topN)
  #high diffits
  dffits.out<-which(dffits(the.linear.model)>2*sqrt(p/n))
  #topN values
  dffits.top<-head(rev(sort(dffits(the.linear.model))),topN)
  #Cook's over 1
  cooks.out<-which(cooks.distance(the.linear.model)>1)
  #topN cooks
  cooks.top<-head(rev(sort(cooks.distance(the.linear.model))),topN)
  #Create a list with the statistics -- cant do a data frame as different lengths 
  list.of.stats<-list(Std.res=res.out,Std.res.top=res.top, Leverage=lev.out, Leverage.top=lev.top, DFFITS=dffits.out, DFFITS.top=dffits.top, Cooks=cooks.out,Cooks.top=cooks.top)
  #return the statistics
  list.of.stats}

outliers.nursery <- show_outliers(first_lm, 5)
influtial.1 <- intersect(outliers.nursery$DFFITS,outliers.nursery$Leverage)
influtial.1

outliers.nursery$DFFITS.top
typeof(outliers.nursery$Leverage.top)
outliers.nursery$DFFITS

nursery_dt[c(18, 28),][,4]

#question 4 
p1<-ggplot(data=nursery_dt, aes(x=Dep,y=NurseryExp))+geom_point()+geom_smooth(method="lm",se=FALSE)
p2<-ggplot()+geom_smooth(data=nursery_dt[-c(18,28),],aes(x=Dep, y=NurseryExp), method="lm", se=FALSE, linetype="dashed")
p1+p2$layers[]

colnames(nursery_dt)
third.lm <- lm(PrimaryExp ~ Dep + factor(MoreSmall), data=nursery_dt)
fourth.lm <- lm(PrimaryExp ~ Dep*factor(MoreSmall), data=nursery_dt)
display(third.lm)
display(fourth.lm)

p3 <- ggplot(data=nursery_dt, aes(x=Dep, y=PrimaryExp, colour=factor(MoreSmall)))+geom_point()+geom_smooth(method="lm",se=FALSE)
p3


predict(third.lm,data.frame(Dep=80,MoreSmall=0)) - predict(fourth.lm,data.frame(Dep=80,MoreSmall=0))
predict(third.lm,data.frame(Dep=18,MoreSmall=1)) - predict(fourth.lm,data.frame(Dep=18,MoreSmall=1))

#Redidual plots
par(mfrow=c(2,2))
plot(third.lm, which=c(1,2))
hist(rstandard(third.lm), freq = FALSE , 
     main="Histogram of standardised residuals", 
     cex.main=0.8, xlab="Standardised residuals")

range(nursery_dt$PrimaryExp)
display(third.lm)
