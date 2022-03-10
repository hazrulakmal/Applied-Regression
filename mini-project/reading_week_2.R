library(car)
library(arm)
library(ggplot2)
library(gridExtra)


rw.data <- read.csv("RWNS_final.csv", header = TRUE,stringsAsFactors = TRUE)
head(rw.data)
summary(rw.data)
##---------plotting include missing points

p1 <- ggplot(rw.data, aes(x= fiveem, y=ks4score)) + geom_boxplot()
p2 <- ggplot(rw.data, aes(x= fiveac, y=ks4score)) + geom_boxplot()
p3 <- ggplot(rw.data, aes(x= factor(k3en), y=ks4score)) + geom_boxplot() + labs(x="English Score Tier")
p4 <- ggplot(rw.data, aes(x= factor(k3ma), y=ks4score)) + geom_boxplot() + labs(x="Maths Score Tier")
p5 <- ggplot(rw.data, aes(x= factor(k3sc), y=ks4score)) + geom_boxplot() +  labs(x="Science Score Tier")
p6 <- ggplot(rw.data, aes(x= gender, y=ks4score)) + geom_boxplot()
p7 <- ggplot(rw.data, aes(x= SECshort, y=ks4score)) + geom_boxplot() 
p8 <- ggplot(rw.data, aes(x= hiquamum, y=ks4score)) + geom_boxplot() 
p9 <- ggplot(rw.data, aes(x= singlepar, y=ks4score)) + geom_boxplot()
p10 <- ggplot(rw.data, aes(x= house, y=ks4score)) + geom_boxplot()
p11 <- ggplot(rw.data, aes(x= fsm, y=ks4score)) + geom_boxplot()
p12 <- ggplot(rw.data, aes(x= parasp, y=ks4score)) + geom_boxplot()
p13<-ggplot(rw.data, aes(x=computer, y=ks4score)) + geom_boxplot()
p14<-ggplot(rw.data, aes(x=tuition, y=ks4score)) + geom_boxplot()
p15<-ggplot(rw.data, aes(x=pupasp, y=ks4score)) + geom_boxplot()
p16<-ggplot(rw.data, aes(x=homework, y=ks4score)) + geom_boxplot()
p17<-ggplot(rw.data, aes(x=attitude, y=ks4score)) + geom_boxplot()
p18<-ggplot(rw.data, aes(x=sen, y=ks4score)) + geom_boxplot()
p19<-ggplot(rw.data, aes(x=truancy, y=ks4score)) + geom_boxplot()
p20<-ggplot(rw.data, aes(x=absent, y=ks4score)) + geom_boxplot()
p21<-ggplot(rw.data, aes(x=exclude, y=ks4score)) + geom_boxplot()
p22<-ggplot(rw.data, aes(x=IDACI_n, y=ks4score)) + geom_point()
p23<-ggplot(rw.data, aes(x=FSMband, y=ks4score)) + geom_boxplot()

p7 <- p7 +  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
p8 <- p8 + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
grid.arrange( p7, p8, ncol =2, nrow=1)

grid.arrange(p1, p2, p3, p4, ncol =2, nrow=2)
grid.arrange(p5, p6, p9, p10,p11,p12, ncol =3, nrow=2)
grid.arrange(p13,p14,p15,nrow=2)
grid.arrange(p16,nrow=1)
grid.arrange(p17,p21,p19,p20,nrow=2)
grid.arrange(p18,p22,p23,nrow=2)

##--- singlepar, house, fsm, parasp, tuition, computer, attitude, exclude, truancy, absent, sen, fsmband

##----change the baselines
rw.data$SECshort <- relevel(rw.data$SECshort, ref = "Routine,_semi-routine_or_unemployed")
rw.data$hiqumum <- relevel(rw.data$hiquamum, ref = "No_qualification")
rw.data$singlepar <- relevel(rw.data$singlepar, ref = "yes")
rw.data$house <- relevel(rw.data$house, ref = "rented")
rw.data$fsm <- relevel(rw.data$fsm, ref = "yes")
rw.data$parasp <- relevel(rw.data$parasp, ref = "No")
rw.data$computer <- relevel(rw.data$computer, ref = "No")
rw.data$tuition <- relevel(rw.data$tuition, ref = "No")
rw.data$homework <- relevel(rw.data$homework, ref = "none")
rw.data$attitude <- relevel(rw.data$attitude, ref = "very_low")
rw.data$sen <- relevel(rw.data$sen, ref = "Yes")
rw.data$truancy <- relevel(rw.data$truancy, ref = "Yes")
rw.data$absent <- relevel(rw.data$absent, ref = "Yes")
rw.data$exclude <- relevel(rw.data$exclude, ref = "Yes")
rw.data$FSMband <- relevel(rw.data$FSMband, ref = "35pr+")

#duplicate data
new.rw.data <- rw.data


##---homework
new.rw.data$new_homework <- new.rw.data$homework
levels(new.rw.data$new_homework)[levels(new.rw.data$new_homework)%in%
                                   c("none","1_evening","2_evenings")] <- "low"
levels(new.rw.data$new_homework)[levels(new.rw.data$new_homework)%in%
                                   c("3_evenings","4_evenings","5_evenings")] <- "high"
summary(new.rw.data$new_homework)
p25<-ggplot(new.rw.data, aes(x=new_homework, y=ks4score)) + geom_boxplot()

##---attitude
new.rw.data$new_attitude <- new.rw.data$attitude
levels(new.rw.data$new_attitude)[levels(new.rw.data$new_attitude)%in%
                                   c("low","very_low")] <- "low"
levels(new.rw.data$new_attitude)[levels(new.rw.data$new_attitude)%in%
                                   c("high","very_high")] <- "high"
summary(new.rw.data$new_attitude)
p27<-ggplot(new.rw.data, aes(x=new_attitude, y=ks4score)) + geom_boxplot()


##---k3ma, k3en and k3sc
cor(new.rw.data$k3en,new.rw.data$k3ma)
cor(new.rw.data$k3en,new.rw.data$k3sc)
cor(new.rw.data$k3sc,new.rw.data$k3ma)
##---the k3s have strong correlation

meank3s <- (new.rw.data$k3en+new.rw.data$k3ma+new.rw.data$k3sc)/3
table(meank3s)
meank3s <- as.factor(meank3s)
new.rw.data$meank3s <- meank3s
summary(new.rw.data)

new.rw.data$new_meank3s <- new.rw.data$meank3s
levels(new.rw.data$new_meank3s)[levels(new.rw.data$new_meank3s)%in%
                                  c("2.33333333333333","2.66666666666667","3","3.33333333333333","3.66666666666667")] <- "low"
levels(new.rw.data$new_meank3s)[levels(new.rw.data$new_meank3s)%in%
                                  c("4","4.33333333333333","4.66666666666667","5","5.33333333333333","5.66666666666667")] <- "medium"
levels(new.rw.data$new_meank3s)[levels(new.rw.data$new_meank3s)%in%
                                  c("6","6.33333333333333","6.66666666666667","7","7.33333333333333")] <- "high"
summary(new.rw.data$new_meank3s)
p24<-ggplot(new.rw.data, aes(x=new_meank3s, y=ks4score)) + geom_boxplot()


##---FSMband
new.rw.data$new_FSMband <- new.rw.data$FSMband
levels(new.rw.data$new_FSMband)[levels(new.rw.data$new_FSMband)%in%
                                  c("<5pr","5pr-9pr","9pr-13pr")] <- "low"
levels(new.rw.data$new_FSMband)[levels(new.rw.data$new_FSMband)%in%
                                  c("13pr-21pr","21pr-35pr","35pr+")] <- "high"
summary(new.rw.data$new_FSMband)
p28<-ggplot(new.rw.data, aes(x=new_FSMband, y=ks4score)) + geom_boxplot()


##---SECshort
new.rw.data$new_secshort <- new.rw.data$SECshort

levels(new.rw.data$new_secshort)[levels(new.rw.data$new_secshort) %in% c("Intermediate","Routine,_semi-routine_or_unemployed")] <- "non-professional"
levels(new.rw.data$new_secshort)[levels(new.rw.data$new_secshort) %in% c("Managerial_and_professional")] <- "professional"

summary(new.rw.data$new_secshort)
p29<-ggplot(new.rw.data, aes(x=new_secshort, y=ks4score)) + geom_boxplot()

##---hiquamum
new.rw.data$new_hiquamum <- new.rw.data$hiquamum

levels(new.rw.data$new_hiquamum)[levels(new.rw.data$new_hiquamum) %in% c("GCE_A_Level_or_equivalent","HE_below_degree_level")] <- "Pre-university-equivalent"

summary(new.rw.data$new_hiquamum)

grid.arrange(p25, p27, p24, p28, ncol =2, nrow=2)

#note
# use meank3s, remove fsm, tuition, new_secshort, idacn 

##---first regression
ks4score.all.lm <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                     + factor(k3en) + factor(k3ma) + factor(k3sc)
                     +factor(gender)+factor(new_secshort)+factor(new_hiquamum)
                     +factor(singlepar)+factor(house)+factor(fsm)
                     +factor(parasp)+factor(computer)+factor(tuition)+factor(pupasp)
                     +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                     +factor(absent)+factor(exclude)+IDACI_n+factor(new_FSMband)
                     ,data=new.rw.data)
Anova(ks4score.all.lm)

##---remove new_secshort which has the largest p-value
ks4score.all.lm2 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)+factor(new_hiquamum)
                      +factor(singlepar)+factor(house)+factor(fsm)
                      +factor(parasp)+factor(computer)+factor(tuition)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(absent)+factor(exclude)+IDACI_n+factor(new_FSMband)
                      ,data=new.rw.data)
Anova(ks4score.all.lm2)

##---remove tuition which has the largest p-value
ks4score.all.lm3 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)+factor(new_hiquamum)
                      +factor(singlepar)+factor(house)+factor(fsm)
                      +factor(parasp)+factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(absent)+factor(exclude)+IDACI_n+factor(new_FSMband)
                      ,data=new.rw.data)
Anova(ks4score.all.lm3)

##---remove fsm which has the largest p-value
ks4score.all.lm4 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)+factor(new_hiquamum)
                      +factor(singlepar)+factor(house)
                      +factor(parasp)+factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(absent)+factor(exclude)+IDACI_n+factor(new_FSMband)
                      ,data=new.rw.data)
Anova(ks4score.all.lm4)

##---remove new_hiquamum  which has the largest p-value
ks4score.all.lm5 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)
                      +factor(singlepar)+factor(house)
                      +factor(parasp)+factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(absent)+factor(exclude)+IDACI_n+factor(new_FSMband)
                      ,data=new.rw.data)
Anova(ks4score.all.lm5)

##---remove parasp  which has the largest p-value
ks4score.all.lm6 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)
                      +factor(singlepar)+factor(house)
                      +factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(absent)+factor(exclude)+IDACI_n+factor(new_FSMband)
                      ,data=new.rw.data)
Anova(ks4score.all.lm6)

vif(ks4score.all.lm6)
##---All remaining predictors are statistical significant at 5% level. 


new.rw.data.2 <- subset(new.rw.data, singlepar!="missing"&house!="other/DK/Ref"&computer!="missing"&
                        homework!="missing"&attitude!="missing"&sen!="missing"&
                        truancy!="missing"&absent!="missing"&exclude!="missing"&
                        FSMband!="NA")
summary(new.rw.data.2)

#check significant predictors
ks4score.all.lm7 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)
                      +factor(singlepar)+factor(house)
                      +factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(absent)+factor(exclude)+IDACI_n+factor(new_FSMband)
                      ,data=new.rw.data.2)
Anova(ks4score.all.lm7)

##---remove IDACI_n  which has the largest p-value
ks4score.all.lm8 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)
                      +factor(singlepar)+factor(house)
                      +factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(absent)+factor(exclude)+factor(new_FSMband)
                      ,data=new.rw.data.2)
Anova(ks4score.all.lm8)

##---remove absent which has the largest p-value
ks4score.all.lm9 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3en) + factor(k3ma) + factor(k3sc)
                      +factor(gender)
                      +factor(singlepar)+factor(house)
                      +factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(exclude)+factor(new_FSMband)
                      ,data=new.rw.data.2)
Anova(ks4score.all.lm9)
vif(ks4score.all.lm9)
##---All remaining predictors are statistical significant at 5% level. 


##--k3ma, k3en, k3sc
ks4score.k3en <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      +factor(k3en)
                      +factor(gender)
                      +factor(singlepar)+factor(house)
                      +factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(exclude)+factor(new_FSMband)
                      ,data=new.rw.data.2)
summary(ks4score.k3en)
##---Multiple R-Squared =  0.6602 RRE = 81.89

ks4score.k3ma <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                   +factor(k3ma)
                   +factor(gender)
                   +factor(singlepar)+factor(house)
                   +factor(computer)+factor(pupasp)
                   +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                   +factor(exclude)+factor(new_FSMband)
                   ,data=new.rw.data.2)
summary(ks4score.k3ma)
##---Multiple R-Squared =  0.6754 RRE = 80.04

ks4score.k3sc <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                   +factor(k3sc)
                   +factor(gender)
                   +factor(singlepar)+factor(house)
                   +factor(computer)+factor(pupasp)
                   +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                   +factor(exclude)+factor(new_FSMband)
                   ,data=new.rw.data.2)
summary(ks4score.k3sc)
##---Multiple R-Squared =  0.6712 RRE = 80.55
##---as k3ma has the highest Multiple R-Squared value + these predictors are highly correlated (correlation & vif), omit k3en & k3sc

ks4score.all.lm10 <-lm(ks4score ~ factor(fiveac)+factor(fiveem)
                      + factor(k3ma)
                      +factor(gender)
                      +factor(singlepar)+factor(house)
                      +factor(computer)+factor(pupasp)
                      +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                      +factor(exclude)+factor(new_FSMband)
                      ,data=new.rw.data.2)
Anova(ks4score.all.lm10)
vif(ks4score.all.lm10)

##---fiveav and fiveem
ks4score.k3ma.fiveac <-lm(ks4score ~ factor(k3ma)+factor(fiveac),data=new.rw.data.2)
summary(ks4score.k3ma.fiveac)
##---Multiple R-Squared = 0.6471

ks4score.k3ma.fiveem <-lm(ks4score ~ factor(k3ma)+factor(fiveem),data=new.rw.data.2)
summary(ks4score.k3ma.fiveem)
##---Multiple R-Squared = 0.5639
##---as fiveac has the highest Multiple R-Squared value, omit fiveem
##--- both predictors are highly correlated (vif)

ks4score.all.lm11 <-lm(ks4score ~ factor(fiveac)
                       + factor(k3ma)
                       +factor(gender)
                       +factor(singlepar)+factor(house)
                       +factor(computer)+factor(pupasp)
                       +factor(new_homework)+factor(new_attitude)+factor(sen)+factor(truancy)
                       +factor(exclude)+factor(new_FSMband)
                       ,data=new.rw.data.2)
Anova(ks4score.all.lm11)
vif(ks4score.all.lm11)


##---truancy, absent and exclude
ks4score.k3ma <-lm(ks4score ~ factor(k3ma) ,data=new.rw.data.2)
summary(ks4score.k3ma)
##---Multiple R-Squared = 0.4842

ks4score.k3ma.truancy <-lm(ks4score ~ factor(k3ma)+factor(truancy),data=new.rw.data.2)
summary(ks4score.k3ma.truancy)
##---Multiple R-Squared = 0.4942


ks4score.k3ma.exclude <-lm(ks4score ~ factor(k3ma)+factor(exclude),data=new.rw.data.2)
summary(ks4score.k3ma.exclude)
##---Multiple R-Squared = 0.5043
##---as exclude has the highest Multiple R-Squared value, omit absent and truancy


ks4score.k3ma.newhom <-lm(ks4score ~ factor(k3ma)+factor(new_homework),data=new.rw.data.2)
summary(ks4score.k3ma.newhom)
##---Multiple R-Squared = 0.4957

ks4score.k3ma.pupasp <-lm(ks4score ~ factor(k3ma)+factor(pupasp),data=new.rw.data.2)
##---Multiple R-Squared = 0.4988
##---as pupasp has the highest Multiple R-Squared value, omit new_homework


#---singlepar, house and computer
ks4score.k3ma.sgp <-lm(ks4score ~ k3ma+factor(singlepar),data=new.rw.data.2)
summary(ks4score.k3ma.sgp)
##---Multiple R-Squared = 0.4899

ks4score.k3ma.house <-lm(ks4score ~ k3ma+factor(house),data=new.rw.data.2)
summary(ks4score.k3ma.house)
##---Multiple R-Squared = 0.4909

ks4score.k3ma.cpt <-lm(ks4score ~ k3ma +factor(computer),data=new.rw.data.2)
summary(ks4score.k3ma.cpt)
##---Multiple R-Squared = 0.4906
##---as house has the highest Multiple R-Squared value, omit singlepar and computer
## The estimate coefficient for the two predictors (house& signlepar) are roughly the same. it means that they have the same relationships. single parents less linkely to own a house. 

# predictor elimination sorely based on r-squared is not solid.I remember she says dont choose model purely based on maximising diagnostic statistics
# comparing computer with singlepar & house does not have a solid reason other than the boxplot
# The end product is still the same but there are some notable different in between
# I use k3ma over other k3s because it has highest r-squared. all k3s are highly correlated(vif, pearson coeff)
# I recon not to use k3mean cus she didnt teach us to combine predictors so iam a bit reluctant on this.
# I got to reject absent through f-test
# fsm sum sq is not 0 
# new_fsmband cannot be rejected if k3ma is treated as a factor but I think it's fine if we treat it as continuous as the model become easier to interpret.


ks4score.all.lm12 <-lm(ks4score ~ factor(fiveac)
                       +factor(k3ma)
                       +factor(gender)
                       +factor(house)
                       +factor(pupasp)
                       +factor(new_attitude)+factor(sen)
                       +factor(exclude)+factor(new_FSMband)
                       ,data=new.rw.data.2)
Anova(ks4score.all.lm12)
vif(ks4score.all.lm12)
summary(ks4score.all.lm12)

final_model <- lm(ks4score ~ factor(fiveac)
                  +k3ma
                  +factor(gender)
                  +factor(house)
                  +factor(pupasp)
                  +factor(new_attitude)+factor(sen)
                  +factor(exclude)
                  ,data=new.rw.data.2)
Anova(final_model)
summary(final_model)


