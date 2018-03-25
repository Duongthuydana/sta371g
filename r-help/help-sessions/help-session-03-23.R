# suggested change
options(digits = 10) # this command tells R to display 10 digits

# Load dataset
cars = read.csv('https://faculty.mccombs.utexas.edu/carlos.carvalho/teaching/Cars.csv')

### Multiple Regression: 
# Use multiple variables by adding them to the right-hand-side
model1 = lm(price ~ mileage + year + featureCount,data=cars)

# summary
summary(model1)

# check assumptions
plot(model1)

### Interactions
model2 = lm(price ~ mileage * year + featureCount,data=cars)
summary(model2)
plot(model2)

### Transformations

# Natural Log - base e
price_ln = log(cars$price)
model3 = lm(price_ln ~ mileage*year + featureCount,data=cars)
summary(model3)
plot(model3)

# Note: inverse function of log() is exp()
exp(log(.45))

# Square root
price_sqrt = sqrt(cars$price)
model4 = lm(price_sqrt ~ mileage*year + featureCount,data=cars)
summary(model4)
plot(model4)

### Predict from Multiple regression

# prediction interval
predict.lm(model2,list(mileage=200000,year=1985,featureCount=14),interval='prediction')

# confidence interval
predict.lm(model2,list(mileage=200000,year=1985,featureCount=14),interval='confidence')


### Using predict.lm for effects from complicated models
# when we have interactions or logs in our model, the effect of a specific change is not as simple as looking at a coefficient estimate in the regression summary. 

# classic car
classic = predict.lm(model2,list(mileage=200000,year=1960,featureCount=14))
# but what if I take it on a road trip?
roadtrip = predict.lm(model2,list(mileage=201000,year=1960,featureCount=14))
# how much value do I lose by roadtripping in this car?
classic-roadtrip
# SIDE NOTE: Does this make sense? Why not?
# Note the range of the data:
summary(cars)
plot(cars$year,cars$mileage)
# dataset only contains cars from 1988-2015
# Our example is way outside the range of the data. It's an extrapolation!


###
### Practice diagnosing regression conditions. 
### (I wouldn't worry about typing this right now)

### Normality

# for side-by-side plots:
par(mfrow=c(1,2))

#!! Obviously not normal:
example = rexp(50)
# histogram
hist(example)
# qqplot
qqnorm(example)
qqline(example)


#!! These are all normal:


example = rnorm(20)
# histogram
hist(example)
# qqplot
qqnorm(example)
qqline(example)

example = rnorm(50)
# histogram
hist(example)
# qqplot
qqnorm(example)
qqline(example)


#!! Now, not normal, but harder to tell:

example = rnorm(50) + rexp(50)
# histogram
hist(example)
# qqplot
qqnorm(example)
qqline(example)

example = rt(50,1.8)
# histogram
hist(example)
# qqplot
qqnorm(example)
qqline(example)

### Equal variance

# back to a single plot
par(mfrow=c(1,1))

# Obvious Problem
x = rnorm(300)+10
plot(x,rnorm(300,0,x^10/50),ylab="Residual")

# Less Obvious
x = rnorm(300)+10
plot(x,rnorm(300,0,x^3/50),ylab="Residual")

# Less Obvious - is there even an issue?
x = rnorm(300)+10
plot(x,rnorm(300,0,20-x),ylab="Residual")

# Equal Variance
x = runif(300)+10
plot(x,rnorm(300),ylab="Residual")

# Equal Variance
x = rnorm(300)+10
plot(x,rnorm(300),ylab="Residual")

# Equal Variance
x = rnorm(50)+10
plot(x,rnorm(50),ylab="Residual")

### Linearity

# back to a double plot
par(mfrow=c(1,2))

# Obvious Problem
x = rnorm(300)+10
y = x^2+rnorm(300,0,1)
plot(x,y)
model = lm(y~x)
plot(x,resid(model),ylab="Residual")


# Less Obvious Problem
x = rnorm(300)+10
y = sqrt(x)+rnorm(300,0,.01)
plot(x,y)
model = lm(y~x)
plot(x,resid(model),ylab="Residual")


# No Problem
x = rnorm(300)+10
y = (x)+rnorm(300,0,.1)
plot(x,y)
model = lm(y~x)
plot(x,resid(model),ylab="Residual")

# No Problem, but no relationship!
x = rnorm(300)+10
y = rnorm(300,0,.1)
plot(x,y)
model = lm(y~x)
plot(x,resid(model),ylab="Residual")

### "Pop Quiz" Which conditions are violated?
x = rnorm(300)+10
y = x*rexp(300)
model = lm(y~x)
#
par(mfrow=c(1,2))
#
plot(fitted(model),resid(model))
abline(h=0,col='grey')
#
qqnorm(resid(model))
qqline(resid(model))


### REVERT back to original plotting option:
par(mfrow=c(1,1))
