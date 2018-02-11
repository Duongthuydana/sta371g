# SAI ARAVINDH RAVI, TEACHING ASSISTANT (STA 371G) 
#OFFICE HOURS WEDNESDAY 2-4pm

#This R tutorial session focuses on a review of probability and how to implement it in R


#Probabilities using R

#Suppose (for simplicity) that the price of IBM's stock can increase $1 or decrease $1. Likewise for General Motors' stock. 
#Let X be the change in the price of IBM's stock and let Y be the change in the price of General Motors' stock. 
#Suppose that (X, Y) have the following joint distribution

# X\Y       -1      +1

# -1        0.25    0.15

# +1        0.05    0.55

#Does this table qualify as a potential joint probability function? Why?

pXdown_Ydown = 0.25
pXdown_Yup = 0.15
pXup_Ydown = 0.05
pXup_Yup = 0.55

#Probability of X going up and down, ignoring the other stock (marginal densities for X)

pXdown = pXdown_Ydown + pXdown_Yup
pXup = pXup_Ydown + pXup_Yup

pXdown
pXup

#Probability of Y going up and down, ignoring the other stock. (marginal densities for Y)

pYdown = pXdown_Ydown + pXup_Ydown
pYup = pXdown_Yup + pXup_Yup

pYdown
pYup

#Conditional probability
#### What is the probability that the stock price of IBM goes up by $1 given that  
####        the stock price of GM has gone down by $1?

# Conditional probability review: P(A/B) = P(AnB)/P(B)

pXup_given_Ydown = pXup_Ydown/pYdown
pXup_given_Ydown

pXup_given_Yup = pXup_Yup/pYup
pXup_given_Yup

#Law of total probability (LoToP): P(E) = P(E/A)*P(A) + P(E/B) * P(B)

#What is the probability of IBM Stock going up by $1? One way to find this is by the marginal 
###  probability values, another way is by LoToP.

pXup = pXup_given_Ydown * pYdown + pXup_given_Yup * pYup
pXup


#Bayes theorem using R

#Example 1
# likelihood = prob of spots given small pox
pSpotsGivenSmallpox <- 0.9
## Note: <- and = signs are equivalent in R

#prior = prob of small pox
pSmallpox <- 0.001

#marginal likelihood = prob of spots
pSpots <- 0.081

#find posterior = prob of small pox given spots = likelihood*prior/marginal likelihood

pSmallpoxGivenSpots = pSpotsGivenSmallpox * pSmallpox / pSpots

#print
pSmallpoxGivenSpots
          

#Example 2
#Try this on your own: 
### The probability of a person getting a particular disease is 0.1. There is a test to identify
### the disease, which has probability of giving a false negative of 0.2. Also, the probability
### of the test showing a positive result, on the whole, is 0.09. What is the probability of
### testing positive, given that a person has the disease? What is the probability of having the disease
### given that I've tested positive?


#likelihood = prob of Positive Test given disease
pPositiveGivenDisease <- 0.8

#prior = prob of disease
pDisease <- 0.1

#marginal likelihood = prob of positive test
pPositive <- 0.09

#posterior = prob of disease given positive test
# posterior (P(B/A)) = likelihood (P(A/B)) * prior P(B) / marginal likelihood (P(A))
##  Full Bayes Formula: P(B/A) = (P(A/B)*P(B))/(P(A/B)*P(B)+P(A/not B)*P(not B))
pDiseaseGivenPositive <- pPositiveGivenDisease * pDisease / pPositive
pDiseaseGivenPositive





