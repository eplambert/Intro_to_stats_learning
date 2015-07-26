# Chapter 3 ~ Linear Regression

# Example
# Important questions that we seek to answer
#    1. Is there a relationship between advertising and sales?
#    2. How strong is the relationship between advertising budget and sales?
#    3. Which media contribute to sales?
#    4. How accurately can we estimate the effect of each media on sales?
#    5. How accurately can we predict future sales?
#    6. Is the relationship linear?
#    7. Is there synergy among advertising media?

# setwd("C:/Users/eric/Desktop/R Projects/Intro_To_Stats")

library(scatterplot3d)
library(rgl)
library(Rcmdr)

# Load advertising data
advertising <- read.csv('Advertising.csv')

attach(advertising)

# fit a linear model of sales onto tv data
y <- lm(Sales ~ TV)
summary(y)
plot(TV, Sales)
abline(y, col="red")

# If Tv budget was $1,000
predict(y, newdata = data.frame(TV = 1000))
#         Note: predict(y, 1000) will not work.

# We want to know how accurate is the sample mean
# taking averages of many means from a sample approximate the population mean
# To estimate how far off a single estimate of population means is
# with respect to sample saize use:
# STANDARD ERROR
se <- function(x)var(x)/length(x)

adv <- data.frame(TV, Radio, Newspaper, Sales)
# summary of the LM gives coefficients, SE, t-value, p-value,
# RSE, R^2, F-statistic
# Multi-variable Linear Regression
y1 <- lm(Sales ~., adv)
summary(y1)
y2 <- lm(Sales ~ TV + Radio + TV * Radio, adv)
y3 <- lm(Sales ~ TV + Radio, adv)

plot3d <- scatterplot3d(TV, Radio, Sales)
plot3d$plane3d(y3)


# To create a correlation matrix

cor(adv)

