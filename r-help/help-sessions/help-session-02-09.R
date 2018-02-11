a = 1
a = a + 1
a
# define a vector
c(1,2,3,4,5,6)
seq(1,6,1)
1:6 

#
die = 1:6


# vector multiplication element wise
die * die
c(1*1, 2*2, 3*3, 4*4,5*5,6*6)

# inner multiplication
die %*% die
6*6+5*5+4*4+3*3+2*2+1*1

# outer multiplication 
die_1 = 1:5
die %o% die_1   
# col vector die with shape (6*1) * row vector die_1 with shape (1*5)


# mean 
mean(die)

# two functions 
round(mean(die))

# rolling the die
sample(x = die, size = 3)
sample(x = die, size = 3)

# 
sample(die,3)
sample(die,3)
sample(size = 3, x = die)

#
sample(x=die, size=6)

# running out of number
sample(x=die, size=7)

# check the function documentation
?sample

# rolling with replacement
sample(die, size = 20, replace = TRUE)

# expectation
expecation = 1*1/6 + 2*1/6 + 3*1/6 + 4*1/6 + 5*1/6 + 6*1/6
expecation

#
result = sample(die, size=10, replace=TRUE)
mean(result)

#
result = sample(die, size=100000, replace=TRUE)
hist(result)

#
sample(die, size=20, replace=TRUE, prob=c(0.9,0.1,0,0,0,0))
#mean(result)

# wrapper, define your own function


# roll() function
roll <- function(){
  die <- 1:6
  dice <- sample(die, size = 1000, replace = TRUE)
  expectation = mean(dice)
  
  return(expectation)
}

roll()


my_function <- function(){
  
  return
  
}

# 
roll_1 <- function(die){
  dice <- sample(die, size=10,replace=TRUE)
}

#
roll_1()  
# 
result = roll_1(die)
result

#
roll_2 <- function(die){
  dice <- sample(die, size=10,replace=TRUE)
}


