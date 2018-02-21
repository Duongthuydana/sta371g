###
### Calculating expected values, variances, and standard deviations
###

probs = c(.25,.25,.5)
values = c(2,-5,1)
EV = sum(probs*values)
variance = sum(probs*(values-EV)^2)
variance
sqrt(variance) #standard deviation!

###
### Regression 
###

homes = read.csv('https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/houses.csv')
model = lm(homes$Price ~ homes$Living.Area)

# Get intercept, slope, p-values, Rsquared, etc.
summary(model)

# Residual plot
plot(homes$Living.Area,resid(model))
abline(h=0,col='red')

# histogram of the residuals
hist(resid(model))
hist(model$residuals) #same

# qqplot
qqnorm(model$residuals)
qqline(model$residuals)
