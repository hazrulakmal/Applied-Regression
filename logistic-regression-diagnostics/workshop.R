#workshop

#question we try to solve using the data
#1. The program’s main goal was to inform a woman of her chances of being a carrier based on serum markers (chemicals in the blood).
#2. Also of interest was whether age should be taken into account.
#3. We also want to know whether ck and h are sufficient to predict carrier status or whether pk is necessary.
#4. Finally we want to know whether the change in water had an effect on the outcomes

dmd.dat<-read.csv("src/dmd.csv",header = TRUE)
head(dmd.dat)
summary(dmd.dat)

#
cent.pred<-function(v){
  cent.v<-v-mean(v)
  cent.v}  

#afterchg is control variable. 

#center
cent.ck<-cent.pred(dmd.dat$ck)#less likely to have a low values as low as 0
cent.h<-cent.pred(dmd.dat$h)
cent.age<-cent.pred(dmd.dat$age)#intrepretation for a newborn if we dont center it 
cent.pk<-cent.pred(dmd.dat$pk)
#ck is a cheaper test than h

#pk regression
dmd.pk.glm<-glm(carrier~cent.age+afterchg+cent.pk,data=dmd.dat,
                family=binomial(link="logit"))
display(dmd.pk.glm)

## ----ck+h regression--------------------------
dmd.ckh.glm<-glm(carrier~cent.age+afterchg+cent.ck+cent.h,data=dmd.dat,
                 family=binomial(link="logit"))
display(dmd.ckh.glm)
qchisq(0.95,3)


ct.op<-function(predicted,observed){ #arguments
  #create the data frame  
  df.op<-data.frame(predicted=predicted,observed=observed)
  #create a table 
  op.tab<-table(df.op)
  #use the prop.table function to obtain the proportions we need:
  #those who were correctly predicted as 0 
  #@position 1,1 in the table of proportions
  obs0.tab<-round(prop.table(op.tab,2)[1,1],2)
  #those who were correctly predicted as 1
  #@position 2,2 in the table of proportions
  obs1.tab<-round(prop.table(op.tab,2)[2,2],2)
  #and put them under the table 
  op.tab<-rbind(op.tab,c(obs0.tab,obs1.tab))
  #name the rows
  rownames(op.tab)<-c("pred=0","pred=1","%corr")
  #name the columns
  colnames(op.tab)<-c("obs=0","obs=1")
  #return the table
  op.tab
}

pred.pk<-as.numeric(dmd.pk.glm$fitted.values>0.5)
#pass the fitted values and the observed values to ct.op
ct.op(pred.pk,dmd.dat$carrier)

#ck+h
pred.ckh<-as.numeric(dmd.ckh.glm$fitted.values>0.5)
ct.op(pred.ckh,dmd.dat$carrier)

#regression for APC
dmd.glm.apc<-glm(carrier~age+ck+h,data=dmd.dat,family=binomial)
display(dmd.glm.apc)

  







