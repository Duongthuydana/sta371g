# R Review #1

# This is a comment. It won't run as code. Use it to organize and annotate your R code. Then save it! So, later we you look at it again, you'll know what you're looking at. 

# keep your R code in a script (up here!). It serves as a way to show your work, see where mistakes happen, and rerun code if ever needed

# to run a line of code that's typed up here, you can click the "Run" button up there^ or use a keyboard shortcut. On mac it's command+enter (command+return). On Windows it's control+R, and I think control+enter works too. 

12

###
### R objects
###

# In R you can name stuff! all letters work, but spaces don't! Instead try "." and "_"
value = 12
value

my.vector = c(1,3,5,7)
my.vector

an_example_matrix = matrix(my.vector,nrow=2,ncol=2)
an_example_matrix

###
### Common functions
###

# sample average of some numbers (in a vector)
mean(my.vector)

# sample standard deviation of some numbers
sd(my.vector)

# add them together!
sum(my.vector)

# multiply them
prod(my.vector)

# Also: basic math function
1+1
1-1
1*2
1/2

# other helpful functions:
log(20) # natural log
sqrt(20)
exp(20) # e^power
3^2 #squared!
abs(-20)

### Data: there's a lot out there. Sometimes it's in packages:

# Download and install package onto your computer (stays within R)
install.packages("car")

# Load package actively in R
library(car)

# "car" contains a dataset called "UN"
# to view the first few lines of UN, try:
head(UN)

# Why just the first few lines? show all of it in the console with: 
UN
# BUT! if dataset is huge, you don't want to see all of it, so try the head function. How do you see the size of the dataset?

# Dimensions of the dataset: number of rows, then number of columns
dim(UN)

# What is the variable "infant.mortality" mean EXACTLY? or "gdp"
?UN

# The help page for anything is found with "?"
?mean 
# note the option "na.rm"
# To remove missing values (called "NA"), include the option na.rm=TRUE or na.rm=T

###
### Regression!
###
# A quick and brief review of doing regression in R
# the function is lm() meaning Linear Model
model = lm(UN$infant.mortality~UN$gdp)
# Note that lm() removes NA's by default. 

# to see the coefficient estimates, you can simply type:
model

# but it's better to see the details with 
summary(model)


###
### Plots and Graphics
###

# Histogram
hist(UN$infant.mortality)
hist(UN$gdp)
# You can specify the number of breaks/bins, or tell it where to start/stop each bin:
hist(UN$gdp,breaks=20)
hist(UN$gdp,breaks=c(0,500,5000,10000,30000,50000))

# Boxplot
boxplot(UN$gdp)
# Side-by-Side boxplots
UN_1 = UN[1:103,]
UN_2 = UN[104:207,]
boxplot(UN_1$gdp,UN_2$gdp)

# Scatterplot (x,y)
plot(UN$gdp,UN$infant.mortality)
# Class question: can you do this with the "~" instead? Yes you can, but remember it's y~x:
plot(UN$infant.mortality~UN$gdp)

###
### Class question: Subset function
###
?subset
UN_low = subset(UN,UN$infant.mortality<40)

###
### class question: what is the "->" symbol ?
###
favorite_number = 56 
# is the same as
favorite_number <- 56


###
### Student request: Some probability
###

# # Independet variables defined:
# A and B are independent events if P(A and B) = P(A)P(B)
# 
# # Disjoint:
# A and B are disjoint events if they NEVER happen together:
# P(A|B) = 0
# P(A| not B) >=0
# 
# # Some Probability rules you'll want to know:
# # Conditional probability defined:
# P(A|B) = P(A and B) / P(B)
# # Bayes Rule:
# P(A|B) = P(B|A)P(A)/P(B)
# # General Additive rule (?):
# P(A or B) = P(A)+P(B)-P(A and B)
# 
# #
# P(A) = P(A|not B) + P(A|B)
# 
# # 
# P(not A) = 1 - P(A)
# P(not A|B) = 1 - P(A|B)




