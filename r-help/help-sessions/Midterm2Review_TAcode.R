###
### Sai - Houses dataset
###


## Multiple linear regression
## y = α + β1 * x1 + β2 * x2 + . . . + βn * xn + Error 

##  In multiple regression, the coefficient of a predictor assesses the effect on Y
##  of a unit increase in the predictor when all other predictors remain the same. 

## Here's an extended houses dataset, where Sales price of houses is predicted using
## appraisal value of the house (thousands of dollars), Square footage (hundreds of sq.ft.)
## and the number of bedrooms

houses_extended = read.csv('https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/HousesExtended.csv')
View(houses_extended)

full_mulitple_model = lm(Price ~ Appraised_Value + Square_Footage + 
                           Number_Bedrooms, data = houses_extended)

summary(full_mulitple_model)

## Which variables are significant and which ones aren't? 

## Reduced model
reduced_multiple_model = lm(Price ~ Appraised_Value + Square_Footage, 
                            data = houses_extended)
summary(reduced_multiple_model)


## R-Squared value has marginally decreased, but the reduced model is still 
## a better model. Why? 

## How do you interpret the intercept of the reduced model?

## Confidence intervals using the confint() statement

## Find the confidence intervals of the predictor variables at 95% 

confint(reduced_multiple_model, level = 0.95)

## Estimate the SalesPrice for the house at 123 Lotus Avenue, 
## which is appraised at $150,000 and is 2500 sqft and give a 90% prediction interval
## for the same house

## The predict.lm() function is used in R for this.
?predict.lm
newdata = data.frame(Appraised_Value = 150, Square_Footage = 25)
predict.lm(reduced_multiple_model, newdata, interval = 'prediction', level = 0.90)

## Consider that the house at 123 Lotus Avenue undergoes expansion, and 
## 500 sq.ft. are being added to the house. 
## Predict the change in Sales Price due to this expansion.


newdata2 = data.frame(Appraised_Value = 150, Square_Footage = 30)
predict.lm(reduced_multiple_model, newdata2)- predict.lm(reduced_multiple_model, newdata)

## What is the difference between prediction and confidence intervals?

## A prediction interval tells you about the distribution of values of the 
##predicted data point, not the uncertainty in determining the population mean. 

## Prediction intervals are generally wider than confidence intervals

## A confidence interval tells you 
## about the uncertainty in determining the population mean.






###
### Han
###

state <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/state.csv", header=T)
View(state)

# slide 
boxplot(Income~Size)
model_10 = lm(Income~Size, data = state)
summary(model_10)
plot(state$Size, state$Income)

model_11 = lm(Income ~ Illiteracy*Murder, data = state)
summary(model_11)

model_12 = lm(Income~Size*Population, data = state)
summary(model_12)

predict(model_12, list(Size='L',Population=101),interval='prediction')- predict(model_12, list(Size='L',Population=100), interval='prediction')


###
### Jared
###


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


###
###
###

cars = read.csv('https://faculty.mccombs.utexas.edu/carlos.carvalho/teaching/Cars.csv')


### Interactions
carsmodel = lm(price ~ mileage * year,data=cars)
summary(carsmodel)
plot(carsmodel)

### Transformations

# Natural Log - base e
log.carsmodel = lm(log(price) ~ mileage*year,data=cars)
summary(log.carsmodel)
plot(log.carsmodel)

