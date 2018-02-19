# R Help Session #4 - Feb 16
# we will cover the following functions: pnorm, mean, var, sd, t.test

# Suggestion for homework: 
options(digits = 10)

###
### Some useful tips
###

# use a colon to get a sequence of integers
# e.g. from 0 to 6
0:6

# Square root
sqrt(4)


###
### Review of pnorm()
###

?pnorm

# These 2 are the same, due to defaults
pnorm(2, mean=0, sd=1) #=P(X<2)
pnorm(2)

###
### get the sample statistics of a vector
###

data_vector = 1:100

# stats:
mean(data_vector)
var(data_vector)
sd(data_vector)
sd(data_vector)^2


###
### t.test
###

?t.test

# test if the true mean of a vector is 0, or not equal to:
t.test(1:10)

# test if the true mean is 5, or not equal to
t.test(1:10,mu=5)

# test if the true mean is 5, or greater than 5. 
t.test(1:10,mu=5,alternative = "greater")

# test if two groups have equal means:
group1 = 1:10
group2 = 4:16
t.test(group1,group2)
# Note that there's a confidence interval included in each! In this case, it's a confidence interval for mu1-mu2

# 90% confidence interval
t.test(group1,conf.level=0.9)

