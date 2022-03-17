library(arm)
library(ggplot2)
library(gridExtra)
library(car)
library(dplyr)

gym.df <- read.table("src/Health.txt", header = TRUE, stringsAsFactors = TRUE)
head(gym.df, 5)
summary(gym.df)

#prejudice boxplot
p1 <- ggplot(gym.df,aes(x = Age, fill = factor(Health))) + geom_boxplot()+ coord_flip() +theme(legend.position = "none")
p2 <- ggplot(gym.df,aes(x = Income, fill = factor(Health))) + geom_boxplot()+ coord_flip()+theme(legend.position = "none")
p3 <- ggplot(gym.df,aes(x = Gender, fill = factor(Health))) +  geom_bar(position = "fill") +  scale_y_continuous(name = "Within group Percentage", labels = scales::percent)+theme(legend.position = "none")
p4 <- ggplot(gym.df,aes(x = Member, fill = factor(Health))) +  geom_bar(position = "fill") +  scale_y_continuous(name = "Within group Percentage", labels = scales::percent)
grid.arrange(p1,p2,p3,p4, nrow=2)

#regression
glm.1 <- glm(Health ~ Member, gym.df, family=binomial(link="logit"))
summary(glm.1)

#prediction table
pred.glm.1<-as.numeric((glm.1$fitted.values>0.5))
glm.dat.1<-data.frame(predicted=pred.glm.1, observed=gym.df$Health)
table(glm.dat.1)

glm.2 <- glm(Health ~. , gym.df, family=binomial(link="logit"))
summary(glm.2)

#plot
health.breaks<-mutate(gym.df, bin=cut(Age,breaks = seq(20, 75, by=5)))
prop <- prop.table(with(health.breaks, table(Health,bin)),2)[2,]
midbin<- seq(22.5, 72.5, by=5)                      
health.bin<-data.frame(midbin,prop)                
health.bin                      

p5<- ggplot(health.breaks,aes(x=Age,y=Health)) + geom_count()
p5<- p5 + geom_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE)                  
p5 <- p5 +  geom_point(data=health.bin, aes(x=midbin,y=prop),shape=2,color="red", size=3)
p5
