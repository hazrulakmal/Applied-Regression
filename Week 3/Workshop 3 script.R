# Week 3 Workshop
# import libarary

library(arm)
library(ggplot2)

Study_habits <- read.csv("Stats_study_habits_slr.csv")
head(Study_habits)
summary(Study_habits)

# before doing any linear regression analysis, make a hyphothesis relationshop first between the predictors and explonatory

#Plotting the data
with(Study_habits,boxplot(Grade~Interesting, 
                          xlab="Interesting", ylab="Grade"))
#with(argument/data, do what you want, x~y mean x on the x-axis and y-axis)

#advantage of ggpplot = WE can add additional features (colours, etc) without intefering the first original code. 
ggplot(Study_habits, aes(x=factor(Interesting), 
                         y=Grade)) + geom_boxplot()


#plotting the linear model 
grade.interesting.lm<-lm(Grade~Interesting,
                         data=Study_habits)

# display funtion shows estimates for intercept and gradient and their standard deviations. 
display(grade.interesting.lm)
summary(grade.interesting.lm)

#we can find the expected grade for each group by plugging in the value into the linear regression equation. 

#continuous variable linear model
with(Study_habits, plot(Study.Skills,Grade,pch=4))
ggplot(Study_habits, aes(x=Study.Skills, y=Grade))+ geom_point()     


grade.studyskills.lm<-lm(Grade~Study.Skills, data=Study_habits) 
display(grade.studyskills.lm)
summary(grade.studyskills.lm)

ggplot(Study_habits, aes(x=Study.Skills, y=Grade))+ geom_point() + geom_smooth(method="lm",se=TRUE)

#Estimate coefficient is signficant
#R square shows how much the predictor extra information to estimate the Y variable
#residual sd < the range of the data


par(mfrow=c(2,2))
plot(grade.studyskills.lm, which=c(1,2))
hist(rstandard(grade.studyskills.lm))
#fitted values (expectation values of the x values that has observed data) are different than prediction value
# constant varience of the redisuals vc fitted - if there's a cone shape in that graph, the constant variance may not be the case. 


# Workshop Quiz
carsales <- read.csv("carsales.csv", header=TRUE)
head(carsales)

carsales_plot <- ggplot(carsales, aes(x=Age, y=Price)) + geom_point()
carsales_plot

carsales_plot<-carsales_plot+xlim(0,11)+ylim(0,140)
carsales_plot

carsales_plot <- carsales_plot + geom_smooth(method="lm",se=TRUE)

#employinh linear model
car_price_lm <- lm(Price~Age, data=carsales)
display(car_price_lm)
summary(carsales)


#bank data
bank_dt <- read.csv("bankMLR.csv", header=TRUE)
head(bank_dt)
summary(bank_dt)


dim(bank_dt)

#boxplot for relatioship hyphothesis
with(bank_dt, boxplot(Salary~Gender, xlab="Gender", ylab=" Salary"))
bank_plot <- ggplot(bank_dt, aes(x=Gender, y=Salary)) + geom_point() + geom_smooth(method="lm", se=TRUE)
bank_plot

experience_salary <- ggplot(data=bank_dt, aes(x=YrsExper, y=Salary))+geom_point() 
experience_salary
experience_salary<- experience_salary + xlab("Years of experience") +labs(title="Plot of years of experience against salary (000$)")
experience_salary <- experience_salary + geom_smooth(method="lm",se=FALSE) + ylab("Salary (000$)")

experience_salary_lm <- lm(Salary~YrsExper, data=bank_dt)
display(experience_salary_lm)

10*1.44 + 59.50

par(mfrow=c(2,2))
plot(experience_salary_lm)
hist(rstandard(experience_salary_lm))
