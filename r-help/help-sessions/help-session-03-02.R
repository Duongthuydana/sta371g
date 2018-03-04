# R review session 03/02/2018
# Sai Aravindh Ravi & Jared Fisher

## We are going to do a recap of simple linear regression and how to implement 
## mulitple linear regression models in R

## Simple Linear Regression
## y = α + β * x + Error 

## The lm function is used to model linear regression in R

houses = read.csv('https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/HouseValues.csv')
View(houses)
?lm
simple_model = lm(houses$SalesPrice ~ houses$AppraisedValue)
simple_model2 = lm(SalesPrice ~ AppraisedValue, data = houses)

## Get slope, intercept, p-values, R-squared values 
summary(simple_model)

## Residual plot
plot(houses$AppraisedValue,resid(simple_model))
abline(h=0,col='red')

## histogram of the residuals
hist(resid(simple_model))
hist(simple_model$residuals) #same

## qqplot
qqnorm(simple_model$residuals)
qqline(simple_model$residuals)

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
?confint
confint(reduced_multiple_model, level = 0.95)

## Estimate the SalesPrice for the house at 123 Lotus Avenue, 
## which is appraised at $150,000 and is 2500 sqft and give a 90% prediction interval
## for the same house

## The predict.lm() function is used in R for this.
?predict.lm
newdata = data.frame(Appraised_Value = 150, Square_Footage = 25)

predict.lm(reduced_multiple_model, newdata = list(Appraised_Value = 150, Square_Footage = 25), interval = 'confidence', level = 0.90)

## What is the difference between prediction and confidence intervals?

## A prediction interval tells you about the distribution of values of the 
##predicted data point, not the uncertainty in determining the population mean. 

## Prediction intervals are generally wider than confidence intervals

## A confidence interval tells you 
## about the uncertainty in determining the population mean.
