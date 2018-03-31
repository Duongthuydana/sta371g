# R help session 03/30/2018
## Sai Aravindh Ravi & Han Jiang

library(fpp2)
install.packages("leaps")
library(leaps)
Soft_Drinks <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/Soft Drinks.csv")[,-1:-4]
summary(Soft_Drinks)
View(Soft_Drinks)
## 
## The variables in the model are as follows: 
## pX is the price of your product, pY is the price of your competitor's product
## Deal_X implies there's a promotion on X and similarly deal_Y 
## “oz_X” are the sales of product X; this variable is Y in the model.
## I'm not going to be using oz_Y as a predictor variable for this session. 

## Aim is to apply forward selection and backward elimination to estimate the correct model
## to predict the log(oz_X), ie, the log of sales of product X using the predictor variables
## deal_X, deal_Y, Log(pX), Log(pY), interaction_X which is deal_X * log(pX) & interaction_Y
## which is deal_Y * log (pY)

## Log transforming variables for better fit (not the focus of this session)

L.yX <- log(Soft_Drinks[,"oz_X"])
L.pX <- log(Soft_Drinks[,"pX"])
L.pY <- log(Soft_Drinks[,"pY"])
deal_X = Soft_Drinks[,"deal_X"]
deal_Y = Soft_Drinks[,"deal_Y"]
interaction_X = deal_X*L.pX
interaction_Y = deal_Y*L.pY

##Full linear regression model

full_model = lm(L.yX ~ deal_X + deal_Y + L.pX + L.pY + interaction_X + interaction_Y)
summary(full_model)

#Null model and information indices

NULL.model <- lm(L.yX ~ 1)
summary(NULL.model)

Inf.Ind1 <- rbind(CV(NULL.model),CV(full_model))
row.names(Inf.Ind1) <- c("NULL","FULL")
Inf.Ind1

##Backward Elimination

#We should drop the variable "deal_Y from the full_model, as it has the highest p-value (0.88450)

reduced_model_1 = lm(formula = L.yX ~ deal_X + L.pX + L.pY + interaction_X + interaction_Y)
summary(reduced_model_1)

Inf.Ind2 <- rbind(CV(NULL.model),CV(full_model), CV(reduced_model_1))
row.names(Inf.Ind2) <- c("NULL","FULL","REDUCED_1")
Inf.Ind2

## The AICc index has decreased. Remove interaction_Y

reduced_model_2 = lm(formula = L.yX ~ deal_X + L.pX + L.pY + interaction_X)
summary(reduced_model_2)  

Inf.Ind3 <- rbind(CV(NULL.model),CV(full_model), CV(reduced_model_1) , CV(reduced_model_2))
row.names(Inf.Ind3) <- c("NULL","FULL","REDUCED_1", "REDUCED_2")
Inf.Ind3

reduced_model_3 = lm(formula = L.yX ~ L.pX + L.pY + interaction_X)
summary(reduced_model_3)

Inf.Ind4 <- rbind(CV(NULL.model),CV(full_model), CV(reduced_model_1) , CV(reduced_model_2), CV(reduced_model_3))
row.names(Inf.Ind4) <- c("NULL","FULL","REDUCED_1", "REDUCED_2", "REDUCED_3")
Inf.Ind4

#Reduced_model_2 is the best model since AICc for the reduced_model_3 model increased

## Forward selection
forward_model_1 = lm(L.yX ~ L.pX)
summary(forward_model_1)

Inf.Ind_forward_1 <- rbind(CV(NULL.model),CV(forward_model_1))
row.names(Inf.Ind_forward_1) <- c("NULL","<FORWARD 1")
Inf.Ind_forward_1

#Iteration 1: Adding second predictor to the model

forward_model_2 = lm(L.yX ~ L.pX + L.pY)
summary(forward_model_2)

forward_model_3 = lm(L.yX ~ L.pX + deal_X)
summary(forward_model_3)

forward_model_4 = lm(L.yX ~ L.pX + deal_Y)
summary(forward_model_4)

forward_model_5 = lm(L.yX ~ L.pX + interaction_X)
summary(forward_model_5) #0.942 (Best)

forward_model_6 = lm(L.yX ~ L.pX + interaction_Y)
summary(forward_model_6)

# Q7: Calculating AICc for the new model
Inf.Ind_forward_2 <- rbind(CV(NULL.model),CV(forward_model_1), CV(forward_model_5))
row.names(Inf.Ind_forward_2) <- c("NULL","FORWARD 1","FORWARD 2")
Inf.Ind_forward_2

#Iteration 2: Adding third predictor to the model
forward_model_7 = lm(L.yX ~ L.pX + interaction_X + L.pY)
summary(forward_model_7) #0.9491 BEST

forward_model_8 = lm(L.yX ~ L.pX + interaction_X + deal_X)
summary(forward_model_8) #0.9426

forward_model_9 = lm(L.yX ~ L.pX + interaction_X + deal_Y)
summary(forward_model_9) #0.9474

forward_model_10 = lm(L.yX ~ L.pX + interaction_X + interaction_Y)
summary(forward_model_10) #0.9474

Inf.Ind_forward_3 <- rbind(CV(NULL.model),CV(forward_model_5), CV(forward_model_7))
row.names(Inf.Ind_forward_3) <- c("NULL","FORWARD 2", "FORWARD 3")
Inf.Ind_forward_3

#Iteration 3: Adding fourth predictor to the model
forward_model_11 = lm(L.yX ~ L.pX + interaction_X + L.pY + deal_X)
summary(forward_model_11) #0.9499 BEST MODEL BECAUSE AICc FOR FORWARD5 INCREASES

forward_model_12 = lm(L.yX ~ L.pX + interaction_X + L.pY + deal_Y)
summary(forward_model_12) #0.9491

forward_model_13 = lm(L.yX ~ L.pX + interaction_X + L.pY + interaction_Y)
summary(forward_model_13) #0.9491

Inf.Ind_forward_4 <- rbind(CV(NULL.model),CV(forward_model_5), CV(forward_model_7), CV(forward_model_11))
row.names(Inf.Ind_forward_4) <- c("NULL","FORWARD 2", "FORWARD 3","FORWARD 4")
Inf.Ind_forward_4

#Iteration 4: Adding fifth predictor to the model
forward_model_14 = lm(L.yX ~ L.pX + interaction_X + L.pY + deal_X + deal_Y)
summary(forward_model_14) #0.9499

forward_model_15 = lm(L.yX ~ L.pX + interaction_X + L.pY + deal_X + interaction_Y)
summary(forward_model_15) #0.9499

#Same R2 values, add any one of the variables. I've added deal_Y

Inf.Ind_forward_5 <- rbind(CV(NULL.model),CV(forward_model_5), CV(forward_model_7), CV(forward_model_11),CV(forward_model_14))
row.names(Inf.Ind_forward_5) <- c("NULL","FORWARD 2", "FORWARD 3","FORWARD 4", "FORWARD 5")
Inf.Ind_forward_5


## The AICc value has increased from -2237.91 to -2235.89, so previous model is the best. (forward model 11) 

## Forward selection and backward elimination using the step() function

step(NULL.model, scope=list(lower=NULL.model, upper= full_model), direction="forward")

step(full_model, scope=list(lower=NULL.model, upper= full_model), direction="backward")

#regsubsets() function
# Auto selection using the nvmax option
subsets_auto = regsubsets(L.yX ~ deal_X + L.pX + L.pY + interaction_X + interaction_Y,
                     data = Soft_Drinks, nvmax = 4)
summary(subsets_auto)

# Regsubsets using backward elimination

subsets_backward = regsubsets(L.yX ~ deal_X + L.pX + L.pY + interaction_X + interaction_Y,
                              data = Soft_Drinks, method = "backward")
summary(subsets_backward)

subsets_forward = regsubsets(L.yX ~ deal_X + L.pX + L.pY + interaction_X + interaction_Y,
                             data = Soft_Drinks, method = "forward")
summary(subsets_forward)


