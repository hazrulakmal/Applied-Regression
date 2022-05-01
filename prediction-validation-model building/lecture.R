library(car)
library(arm)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

data <- read.csv("data/boston.csv", stringsAsFactors = TRUE)
head(data, 5)
colnames(data)
summary(data)

#CHaS RAD categorical

special.dat <- gather(data = data[,-c(4,9)], -MV, key="var", value="value")
p1 <- ggplot(special.dat, aes(x=value, y = MV)) + geom_point(size = 0.5) + geom_smooth(method ="lm", "se"= FALSE) + facet_wrap(~var, scales = "free_x")
p1
