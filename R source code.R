# Quiz 1 - Ex. 3
P_AvB <- 0.17
P_B <- 0.12
P_AyB <- 0.06

P_A <- P_AvB - P_B + P_AyB
P_A

# Quiz 1 - Ex. 3
# This density is a box of inferior lenght 1. The point so that the area below it is  
qunif(0.75)



# Homework 2 - Week 2 - Ex 7
## Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed with a mean of 80 (mm Hg) and a standard deviation of 10.
## What is the probability that a random 35-44 year old has a  DBP greater than 100 (mm Hg)?

#One way to calculate this probability is 

targetDBP <- 100
mu <- 80
sigma <- 10
percentage <- (1-pnorm(targetDBP, mean = mu, sd = sigma)) * 100
percentage

[1] 2.275013%

# while a more succinct option is simply

pnorm(100, mean = 80, sd = 10, lower.tail = FALSE)

[1] 0.02275013



# Homework 2 - Week 2 - Ex 8
## Brain volume for adult women is about 1,100 cc for women with a standard deviation of 75 cc. About what brain volume representes the 10th percentile?
qtile <- .10
mu <- 1100
sigma <- 75

targetVolume <- qnorm(qtile, mean = mu, sd = sigma)
targetVolume

[1] 1003.884



# Homework 2 - Week 2 - Ex 9
## Return to the previous question. Brain volume for adult women is about 1,100 cc for women with a standard deviation of 75 cc. 
## Consider the sample mean of 144 random adult women from this population. Around what is the 10th percentile of the distribution of 
## the distribution of sample means of 144 women?

qtile <- .1
mu <- 1100
sigma <- 75
N <- 144
s_variance <- sigma/sqrt(N)
round(qnorm(qtile, mean = mu, sd = s_variance))
------
[1] 1092
