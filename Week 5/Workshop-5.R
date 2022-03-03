#weeek 5 Outliers

library(car)
library(ggplot2)
library(arm)


study_habits <- read.csv("Stats_study_habits.csv", , header = TRUE,stringsAsFactors = TRUE)
grade.lm.2<-lm(Grade~Study.Skills*factor(Interesting),data=study_habits)
display(grade.lm.2)

#plot the linear model
p1<-ggplot(study_habits, aes(x=Study.Skills, y=Grade,  color=factor(Interesting)))
p1<- p1 + geom_point(size=2) 
p1<-p1+geom_smooth(method="lm", se=FALSE)
#legend
p1<-p1+scale_colour_discrete(name  ="Interesting",
                             breaks=c("0", "1"),
                             labels=c("No", "Yes"))
p1


p2<-ggplot(study_habits, aes(x=Study.Skills, y=Grade, shape=factor(Interesting), color=factor(Interesting)))
p2<- p2 + geom_point(size=2)  + theme_bw()
p2<-p2+geom_smooth(method="lm", se=FALSE)
p2<-p2+scale_colour_grey(name  ="Interesting",
                         breaks=c("0", "1"),
                         labels=c("No", "Yes"))
p2<-p2+scale_shape_discrete(name  ="Interesting",
                            breaks=c("0", "1"),
                            labels=c("No", "Yes"))
p2

#checking the outliers
out.1<-which(study_habits$Grade==min(study_habits$Grade))
out.2<-with(study_habits, which(Grade<60 & Study.Skills<10))
out.3<-with(study_habits, which(Grade>60 & Study.Skills<15))
study_habits[c(out.1,out.2,out.3),c(2,7,8)]

#display regression with outlier removal
Study_habits.out.1<-study_habits[-out.1,]
grade.lm.no.out<-lm(Grade~Study.Skills*factor(Interesting),data=Study_habits.out.1)
display(grade.lm.no.out)

#plot the visual
p3<-ggplot(study_habits, aes(x=Study.Skills, y=Grade,  color=factor(Interesting)))
p3<- p3 + geom_point(size=2) + theme_bw()
p3<- p3 + geom_point(data=study_habits[out.1,], color="black")
p3<-p3+geom_smooth(method="lm", se=FALSE)
p3<-p3+scale_colour_discrete(name  ="Interesting",
                             breaks=c("0", "1"),
                             labels=c("No", "Yes"))
p3<-p3+ geom_smooth(data=Study_habits.out.1, aes(x=Study.Skills, y=Grade,  color=factor(Interesting)), method = "lm",se=FALSE, linetype="dashed")
p3


Study_habits.out.1<-study_habits[-out.1,]
p1<-ggplot(study_habits, aes(x=Study.Skills, y=Grade, shape=factor(Interesting), color=factor(Interesting)))  + labs(tag = "A")
p1<- p1 + geom_point(size=2)
p1<- p1 + geom_point(data=study_habits[out.1,], shape=5, size=3)
p1<-p1+geom_smooth(method="lm", se=FALSE)+ theme_bw()+theme(legend.position = "none")
p1<-p1+scale_colour_grey(name  ="Interesting",
                         breaks=c("0", "1"),
                         labels=c("No", "Yes"))
p1<-p1+scale_shape_discrete(name  ="Interesting",
                            breaks=c("0", "1"),
                            labels=c("No", "Yes"))
p1

p4<-ggplot(study_habits, aes(x=Study.Skills, y=Grade, shape=factor(Interesting), color=factor(Interesting))) + labs(tag = "B")
p4<- p4 + geom_point(data=study_habits[out.2,], size=3, shape=5)
p4<- p4 + geom_point(size=2)
p4<-p4+geom_smooth(method="lm", se=FALSE)+ theme_bw()+theme(legend.position = "none")
p4<-p4+scale_colour_grey(name  ="Interesting",
                         breaks=c("0", "1"),
                         labels=c("No", "Yes"))
p4<-p4+scale_shape_discrete(name  ="Interesting",
                            breaks=c("0", "1"),
                            labels=c("No", "Yes"))

p4
#outliers are real data point. we remove only if it's a typo. 
#if cant justify outlier removal. do double regression, one with and one without the outliers. dicuss them
#compare outliers with the average value
#discuss why they are outliers, then run double regressions and discuss them.

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




#Outlier Statistics
Hourly_Pay<-read.csv("Large_Hourly_Pay.csv",header=TRUE, row.names = NULL)
HP.lm<-lm(log(hourpay)~age+factor(gender)+HighestQual,data=Hourly_Pay)
display(HP.lm)
ggplot(Hourly_Pay, aes(x=age,y=log(hourpay)))+geom_point()

#apply outlier statistics funtion
HP.out<-show_outliers(HP.lm,10)
head(HP.out, 5)

print("Standardised residuals")
HP.out$Std.res.top
print("Leverage values")
HP.out$Leverage.top
print("DFFITS")
HP.out$DFFITS.top
print("Cook's d")
HP.out$Cooks.top


common.out<-intersect(intersect(HP.out$Std.res,HP.out$DFFITS),HP.out$Leverage)
common.out 




































