# Quiz 1 - Ex. 3
## Consider influenza epidemics for two parent heterosexual families. Suppose that the probability is 17\% that at least one of the parents has contracted the disease. The probability that the father has contracted influenza is 12\% while the probability that both the mother and father have contracted the disease is 6\%. What is the probability that the mother has contracted influenza?
P_AvB <- 0.17
P_B <- 0.12
P_AyB <- 0.06

P_A <- P_AvB - P_B + P_AyB
P_A

In [1]: 0.11

-------

# Quiz 1 - Ex. 3
## A random variable, $X$ is uniform, so that it's density is $f(x) = 1$ for $0\leq x \leq 1$. What is it's 75th percentile? Express your answer to two decimal places.
# This density is a box of inferior length 1. The point so that the area below it is  
qunif(0.75)

In [1]: 0.75

-------

# Homework 2 - Week 2 - Ex 7
## Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed with a mean of 80 (mm Hg) and a standard deviation of 10. What is the probability that a random 35-44 year old has a  DBP greater than 100 (mm Hg)?

#One way to calculate this probability is 

targetDBP <- 100
mu <- 80
sigma <- 10
percentage <- (1-pnorm(targetDBP, mean = mu, sd = sigma)) * 100
percentage

In [1]: 2.275013%

# while a more succinct option is simply

pnorm(100, mean = 80, sd = 10, lower.tail = FALSE)

In [1]: 0.02275013

-------

# Homework 2 - Week 2 - Ex 8
## Brain volume for adult women is about 1,100 cc for women with a standard deviation of 75 cc. About what brain volume representes the 10th percentile?
qtile <- .10
mu <- 1100
sigma <- 75

targetVolume <- qnorm(qtile, mean = mu, sd = sigma)
targetVolume

In [1]: 1003.884

-------

# Homework 2 - Week 2 - Ex 9
## Return to the previous question. Brain volume for adult women is about 1,100 cc for women with a standard deviation of 75 cc. Consider the sample mean of 144 random adult women from this population. Around what is the 10th percentile of the distribution of the distribution of sample means of 144 women?

qtile <- .1
mu <- 1100
sigma <- 75
N <- 144
s_variance <- sigma/sqrt(N)
round(qnorm(qtile, mean = mu, sd = s_variance)) 

In [1]: 1092

-------

# Quiz 2 - Week 2 - Exercise 7
## Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed with a mean of 80 (mm Hg) and a standard deviation of 10. What is the probability that a random 35-44 year old has a  DBP less than 70?

pnorm(70,mean=80,sd=10,lower.tail = TRUE)

In [1]: 0.1586553

-------

# Quiz 2 - Week 2 - Exercise 8
## Brain volume for adult women is normally distributed with a mean of about 1,100 cc for women with a standard deviation of 75 cc. About what brain volume represents the 95th percentile?

qnorm(0.95,mean=1100,sd=75,lower.tail = TRUE)

In [1]: 1223.364

-------

# Quiz 2 - Week 2 - Exercise 9
## Return to the previous question. Brain volume for adult women is about 1,100 cc for women with a standard deviation of 75 cc. Consider the sample mean of 100 random adult women from this population. Around what is the 95th percentile of the distribution of that sample mean?

qnorm(0.95,mean=1100,sd=75/10,lower.tail = TRUE)

In [1]: 1112.336

-------

# Quiz 2 - Week 2 - Exercise 12
## You flip a fair coin 5 times, what's the probability of getting 4 or 5 heads?

pbinom(3,size=5,prob=0.5, lower.tail = FALSE)

In [1]: 0.1875

-------

# Quiz 2 - Week 2 - Exercise 13.
## The respiratory disturbance index (RDI), a measure of sleep disturbance, for a specific population has a mean of 15 (sleep events per hour) and a standard deviation of 10. They are not normally distributed. Give your best estimate of the probability that a sample mean RDI of 100 people is between 14 and 16 events per hour?
mu <- 15
sigma <- 10
n <- 100
SE <- sigma/sqrt(n)

left <- 14
right <- 16

percentageLeft <- pnorm(left, mean = mu, sd = SE) * 100
percentageRight <- pnorm(right, mean = mu, sd = SE) * 100

probPercentage <- round(percentageRight - percentageLeft)
probPercentage

In [1]: 68

-------

# Homework 3 - Week 3 - Exercise 1
## Researchers are studying the relative concentration of blood lead for factory workers. They took the natural logarithm of ratio of blood lead concentration for 8 factory workers and 8 control subjects. The measurements resulted in a mean log concentration of 6 (log parts per volume) for the factory workers and 4 for the control subjects. The sample variance in the factory workers was 3 while it was 5 in the control group. Assuming equal variances, create a 95\% confidence interval for the difference in the population means of log blood lead concentration between factory workers and controls (Factory works - Controls).
barX <- 4
barY <- 6
nX <- 8
nY <- 8
SE1 <- 3
SE2 <- 5
inv <-sqrt((1/nX)+(1/nY))
Sp <- sqrt((nX-1)/(nX+nY-2)*SE1 + (1-(nX-1)/(nX+nY-2))*SE2)
cat('The pooled sample variance is', Sp)

qtileC95 <- qt(c(.975), df = nX+nY-2)
cat('\n The 95th t-quantile for a t-distribution with 14 dof is',qtileC95)

lowerBound <- barY - barX - qtileC95 * Sp * inv
higherBound <- barY - barX + qtileC95 * Sp * inv
cat('\n The 95th interval is [',lowerBound,higherBound,']')

In[1]: The pooled sample variance is 2
In [2]: The 95th t-quantile for a t-distribution with 14 dof is 2.144787
In [3]: The 95th interval is [ -0.1447867 4.144787 ]
[Execution complete with exit code 0]
