# y becomes the dummy variable. 
# intead of predict 0 or 1
# p(x) = b0 + b1*x 
# logistic regression model probability of being yes
# p(x) = e(b0+b1*x)/ (1+e(b0+b1*x))

# P1: Why not linear regression
# write as odd function log(p/1-p) = b0+b1*x
b0=1
b1=2
# regular curve
curve(b0+b1*x, from=-5, to=5)
# logistic curve
curve(exp(b0+b1*x)/(1+exp(b0+b1*x)), from=-5,to=5)

# p/(1-p) is the odds from 0 to inf 
# the name from this log(p/1-p) = b0+b1*x  # in form of logit of odds, the relationship become linear

# making prediction

# exploration the dataset 
# gradschool <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
gradschool <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/gradschool.csv")

View(gradschool)
# gre from 220 - 800, 
# gpa from 2.26 - 4.00
# where rank is the undergrad school ranking from 1-4, 1 is the highest
#write.csv(admit_grad,file='E:/Dropbox/F_Disk/1_TA/R_help/R_help.csv',row.names=FALSE)
summary(gradschool)
# 

# simple log regression
mod1<- glm(admit ~ gpa, data = gradschool, family = "binomial")
summary(mod1)
# deviance is a measure of goodness of fit of the model, higher number indicates bad fit
# Question, which one is signifiant.

# Question, what is the intercept of the log of odds
predict(mod1,list(gpa=3), type = 'response') # 0.3096
predict(mod1, list(gpa=3.8), type = 'response') # 0.4101

# psudo-R2
# 1 -(Residual deviance/ null deviation) 
1 - (486.97/499.98) # 0.02602104 

# AIC
AIC(mod1) # 490.9676



# multiple variable models 
mod2 <- glm(admit ~ gre+gpa, data = gradschool, family = "binomial")
summary(mod2) 
# Question: which one is significant

# prediction
predict(mod2,list(gpa=3,gre=400), type = 'response') # 0.1667
predict(mod2,list(gpa=3,gre=780), type = 'response') # 0.357419
predict(mod2,list(gpa=3.8,gre=780),  type = 'response') # 0.50429

1 - (480.34/499.98) #  0.0393
AIC(mod2) # 486.344

# dummy variable
gradschool$rank <- factor(gradschool$rank)
mod3 = glm(admit ~ gre+gpa+rank, data = gradschool)
summary(mod3)

predict(mod3,list(gre=780,gpa=3.8,rank='1'), type='response') # 0.66718
# try 
predict(mod3,list(gre=780,gpa=3.8,rank='4'), type='response') # 0.3441626

# estimate the performance of the model 
# Question: the ratio of predict admission

# the ratio in the dataset not admit
sum(gradschool$admit==0)/ nrow(gradschool) # 0.6825 # most of them didn't get admit

# try mod1

predicted.admit1 = predict(mod1,type = 'response')>0.5
sum(predicted.admit1 == gradschool$admit) / nrow(gradschool) # 0.6825

# try mod3

predicted.admit3 = predict(mod3, type = 'response')>0.5
sum(predicted.admit3 == gradschool$admit) / nrow(gradschool) # 0.715


# Question 90% prediction interval for probability of someone with gpa = 3.8, the lower bound 
#qnorm(0.95) # z = 1.64
#predict.glm(mod1,list(gpa=3.8), type = 'response', level=0.9, se.fit=T)
# the lower bound
#0.410143 - 0.0368*1.64 # pred - z * se








