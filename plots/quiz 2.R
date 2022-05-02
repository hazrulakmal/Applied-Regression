
#load dataset
Height.dat<-read.table("heightmenwomen.txt",header=TRUE)

summary(Height.dat)

#split vegetarians
veg.Height.dat<-subset(Height.dat,vegetarian=="yes")
dim(veg.Height.dat)

#question 3
aggregate(Height.dat$women,Height.dat$vegetarian,mean)
aggregate(Height.dat[2],Height.dat[3],mean)

#question 4
#load libarary

library(ggplot2)
ggplot(data=Height.dat, aes(x=men,y=women))+geom_point()


#Question 5
p1<-ggplot(data=Height.dat, aes(x=vegetarian,y=women))+geom_boxplot()
p2<-ggplot(data=Height.dat, aes(x=vegetarian,y=men))+geom_boxplot()
grid.arrange(p1,p2,nrow=1) 

#Question 6
with(Height.dat, mean(women))

#Question 7 
with(Height.dat, t.test(women, mu=163))


#Wuestion 10 
p3<-ggplot(Height.dat, aes(x = men)) +  geom_histogram(position = "identity", bins = 10, alpha = 0.4, fill="red")
p3<- p3 + geom_histogram(aes(x=women), bins=10,alpha=0.4, fill="blue")
p3

#question 11 
with(Height.dat, sd(women))
with(Height.dat, sd(men))


with(Height.dat, t.test(men,women, var.equal=TRUE, paired=FALSE))
