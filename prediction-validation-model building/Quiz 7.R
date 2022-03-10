#Quiz 7

library(arm)
library(ggplot2)
library(tidyr)
library(car)
library(gridExtra)


#load the data
birth.weight <- read.csv("data/low_birthweight.csv", header = TRUE, stringsAsFactors = TRUE)
summary(birth.weight)
