library(arm)
library(ggplot2)
library(ggpubr)

df <- read.csv("YNEET.csv")
head(df, 5)

par(mfrow=c(1,4))
spend <- ggplot(df, aes(x= SpendStu, y= YNEET, label=Country))+geom_point() +geom_text(aes(label=Country),hjust=0, vjust=0,size=2.5)
gini <- ggplot(df, aes(x= GINI, y= YNEET, label=Country))+geom_point() +geom_text(aes(label=Country),hjust=0, vjust=0,size=2.5)
gdp <- ggplot(df, aes(x= GDP, y= YNEET, label=Country))+geom_point() +geom_text(aes(label=Country),hjust=0, vjust=0,size=2.5)
pop <- ggplot(df, aes(x= Pop.10E6., y= YNEET, label=Country))+geom_point() +geom_text(aes(label=Country),hjust=0, vjust=0,size=2.5)
ggarrange(spend, gini, gdp, pop, ncol =2, nrow=2)

colnames(df)

yyet_lm <- lm(YNEET~SpendStu+Pop.10E6.+GINI+GDP, data=df)
display(yyet_lm)
