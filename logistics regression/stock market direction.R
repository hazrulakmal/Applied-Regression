#Logistics Regression

# used for classification problem that is when the output y = [0,1]
# logit function logit(x1,x2) = 1 - exp-(a*xi + b*x2 + c) 
# non linear model (family of GLMs) so interpretation is different than we used to when explaining linear regression model
# the probability of a success conditional of baseline levels(categorical)/0 (continuous) is the inverselogit intercept
# use ratio risk - find two success probability conditioning on two desire inputs.ratio them 
# interpret them as a percentage change. ex risk ratio = 0.98. a decrease of 2 percent in success from x=1 to x=2 

#The Stock Market Data
# goal is to predict Direction (a qualitative response) using available features (percentage change of return)

library(ISLR2)
names(Smarket)
summary(Smarket)

cor(Smarket[,-9])
#the correlation between lag variables and today's return is close to zero indicating little correlation between them. 

lm.1 <- glm(Direction ~. -Year-Today, data = Smarket, family = binomial)
summary(lm.1)
# The coefficient of lag 1 is negative indicating that if yesterday's return is postie, it is less likely for the return to go up  today. 
#However, at a value 0.15, the p value is  still relatively large and so there is no clear evidence of a real association between lag1 and Direction

summary(lm.1)$coef

lm.prob <- predict(lm.1, type = "response")
lm.prob[1:10] #probabilities

lm.pred <- rep("Down", 1250)
lm.pred[lm.prob > .5] = "Up"

#produce confusion matrix
table(lm.pred, Smarket$Direction)
mean(lm.pred == Smarket$Direction)
#logistic regression correctly predicted the movement of the market 52.2 % of the time.

#cross-validation
train <- (Smarket$Year < 2005)
S.market.2005 <- Smarket[!train,]

Direction.2005 <- Smarket$Direction[!train]

lm.2 <- glm(Direction ~. -Today-Year, data = Smarket, family = binomial, subset = train)

lm.prob.2 <- predict(lm.2, S.market.2005, type = "response")

lm.pred.2 <- rep("Down", 250)
lm.pred.2[lm.prob.2 > .5] = "Up"

table(lm.pred.2, Direction.2005)

mean(lm.pred.2 == Direction.2005)
#our model performs no better than a naive approach. training error is 1-correct rate
#removing the variables that appear not to be helpful might help improve the predictive performance (omitting noise)
#Even if  we manage to improve the model by refitting it with fewer variables (only include the statistically sign). 
#We still have to do further analysis ato investigate whether the small improvement was real or due to random chance. 




