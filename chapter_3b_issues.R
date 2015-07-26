# Chapter 3 ~ Linear regression.
# Looking into potential issues with linear regressions and
# how these issues can be resolved.

# setwd("C:/Users/eric/Desktop/R Projects/Intro_To_Stats")

# Auto data contains:
# mpg, cylinders, displacement, horsepower, weight, acceleration
# year, origin, name

auto <- read.csv("Auto.csv")

attach(auto)

plot(horsepower, mpg)

# Viewing the plot it is non-linear

#comparing various linear models to view statistics
y1 <- lm(mpg ~ horsepower)
y2 <- lm(mpg ~ horsepower + I(horsepower^2), auto)
y3 <- lm(auto$mpg ~ auto$horsepower + I(auto$horsepower^5))

# visualizing the data
abline(y1, col="red")   # viewing the linear regression
lines(lowess(mpg ~ horsepower, f = 0.8), col="blue")    # a non-parametric fit of the data


#####################################################
# Issue 1:  Non-Linear Data
#####################################################

# We see from above that the data is not linear

# must find the residuals of the predictive models
fit1 <- predict(y1, newdata = data.frame(auto$horsepower))
error1 <- mpg - fit1   # finding the error term
plot(fit1, error1)    # ideal: no disercernable pattern.
lines(lowess(error1 ~ fit1, f = 0.8), col="blue")   # a pattern indicates an error with the model

# analyzing the quadratic fit
fit2 <- predict(y2, newdata = data.frame(auto$horsepower))
error2 <- mpg - fit2
plot(fit2, error2)    # ideal: no disercernable pattern.
lines(lowess(error2 ~ fit2, f = 0.8), col="blue")   # notice only a slight pattern

# thus far we can conclude that the 2nd model is better than the first

# checking to see if horsepower^5 is any better. The below indicates that
# it is not a better model
fit3 <- predict(y3, newdata = data.frame(auto$horsepower))
error3 <- mpg - fit3
plot(fit3, error3)    # ideal: no disercernable pattern.
lines(lowess(error3 ~ fit3, f = 0.8), col="blue")

# Since the residual plots indicate a non-linearaity. We can apply
# a non-linear transformation, i.e. log, sqrt, x^2
mpg_log <- log10(mpg)
horsepower_log <- log10(horsepower)
y1_log <- lm(mpg_log ~ horsepower_log)
summary(y1_log)
plot(horsepower_log, mpg_log)
abline(y1_log, col="red")

# Checking the residual plots. The resulting ouput is pretty good.
fit1_log <- predict(y1_log, newdata = data.frame(horsepower_log))
error1_log <- mpg_log - fit1_log   # finding the error term
plot(fit1_log, error1_log)    # ideal: no disercernable pattern.
lines(lowess(error1_log ~ fit1_log, f = 0.8), col="blue") 

###########################################################
# Issue 2:  Correlation of Error Terms
###########################################################

###########################################################
# Issue 3: Non-constant Variance of Error Terms
###########################################################

###########################################################
# Issue 4: Outliers
###########################################################

###########################################################
# Issue 5: High Leverage Points
###########################################################

###########################################################
# Issue 6: Colinearity
###########################################################
