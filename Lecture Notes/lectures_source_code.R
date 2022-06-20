# Confidence intervals for small samples using chi-squared distribution quantiles
n <- 513
mean <- 1150.315
s2 <- 105.977^2 

alpha <- .05 
lower_qtile <- qchisq(alpha/2, n-1)
upper_qtile <- qchisq(1-alpha/2, n-1)

upper_bound <- (n-1)*s2/lower_qtile
lower_bound <- (n-1)*s2/upper_qtile

print(sqrt(lower_bound))
print(sqrt(upper_bound))

In [1]:  99.86484 112.89216

# or more succinctly 

n <- 513
mean <- 1150.315
s2 <- 105.977^2 

alpha <- .05 
qtiles <- qchisq(c(alpha/2, 1-alpha/2), n-1)
ci <- rev((n-1)*s2/qtiles)
sqrt(ci)

In [1]: 99.86484 112.89216


----------
# We can plot a the 95th percentile of the standard normal distribution with the following R routine:
xval <-  seq(-3.2, 3.2, length=1000)
yval <- dnorm(xval)
plot(xval, yval, type="l", axes=TRUE, frame=FALSE, lwd=3, xlab="", ylab="")
x <- seq(qnorm(.95), 3.2, length=1000)
polygon(c(x,rev(x)), c(dnorm(x), rep(0, length(x))), col="salmon")
text(mean(x), mean(dnorm(x))+0.2, "5%", cex=2)
text(qnorm(.95), .01, "1.645",cex=2)


----------
# Two sided tail test for a Z-statistic (under the null hypothesis)

xval <-  seq(-3.2, 3.2, length=1000)
yval <- dnorm(xval)

#plot(xval, yval, type = "l", axes=TRUE, frame=TRUE, lwd = 3, xlab="", ylab= "")
#x <- seq(qnorm(.95), 3.2, length = 100)
#polygon(c(x, rev(x)), c(dnorm(x), rep(0,length(x))), col="red")
#text(mean(x), mean(dnorm(x))+0.2, "5%", cex=2)
#text(qnorm(.95), .01, "1.645",cex=2)

plot(xval, yval, type = "l", axes=TRUE, frame=FALSE, lwd = 3, xlab="", ylab= "")
x <- seq(qnorm(.975), 3.2, length = 100)
polygon(c(x, rev(x)), c(dnorm(x), rep(0,length(x))), col="salmon")
text(mean(x), mean(dnorm(x))+0.2, "2.5%", cex=2)
text(qnorm(.975), .01, "1.96",cex=2)

x <- seq(-3.2, qnorm(0.025), length=100)
polygon(c(x, rev(x)), c(dnorm(x), rep(0,length(x))), col="salmon")
text(mean(x), mean(dnorm(x))+0.2, "2.5%", cex=2)
text(qnorm(.025), .01, "1.96",cex=2)
text(0, dnorm(0)/5, "95%",cex=2)


-------------- 
#Student's two sided test 

xval <-  seq(-4, 4, length=1000)
yval <- dt(xval, 15)
plot(xval, yval, type = "l", axes=TRUE, frame=FALSE, lwd = 3, xlab="", ylab= "")
x <- seq(qt(.975, 15), 4, length=100)
polygon(c(x, rev(x)), c(dt(x,15), rep(0,length(x))), col="salmon")
text(mean(x), mean(dt(xval, 16-1))+0.2, "2.5%", cex=2)
text(qt(.975,15), .01, "2.13",cex=2)
x <- seq(-3.2, qt(.025,16), length=100)
polygon(c(x, rev(x)), c(dt(x,15), rep(0,length(x))), col="salmon")
text(mean(x), mean(dt(xval, 16-1))+0.2, "2.5%", cex=2)
text(qt(.025,15), .01, "2.13",cex=2)
text(0, dt(0,15)/5, "95%", cex=2)


--------------
#Calculating a P-value for a Student's T statistic

pt(0.8, 15, lower.tail=FALSE)
xval <-  seq(-4, 4, length=1000)
yval <- dt(xval, 15)
plot(xval, yval, type = "l", axes=TRUE, frame=FALSE, lwd = 3, xlab="", ylab= "")
x <-  seq(.8, 4, length=100)
polygon(c(x, rev(x)), c(dt(x,15), rep(0,length(x))), col="salmon")
text(mean(x), mean(dt(xval, 16-1))+0.2, "22%", cex=2)
text(.8, .01, "0.8",cex=2)


---------------
# Monte Carlo calculation for a Gossett's T power test for an RDI experiment

no_sim <- 100000 # number of simulation to perform 
n_dof <- 16 # number of degrees of freedom
sigma <- 4 # variance 
mu0 <- 30 # RDI mean under the null hypothesis
mua <- 32 # RDI mean under the alternative hypothesis
z <- rnorm(no_sim) # rnorm is the R function that simulates random variates having a specified normal distribution
chisq <- rchisq(no_sim, df = n_dof - 1) # rchisq is the R function that simulates random variates having a specified chi squared distribution
t_qt <- qt(.95, n_dof-1) # 95th quantile for the Gossett's T distribution
mean(z + sqrt(n_dof)*(mua-mu0)/sigma > 
    t_qt/sqrt(n_dof-1)*sqrt(chisq))
mean 


-----------------
# Calculating a paired T test
diff <- test2 - test1  #pair difference
n <- sum(!is.na(diff)) #number of subjects: 49
mean(diff) #mean of the pair difference: 2.88
sd(diff) #standard deviation of the pair difference: 7.61
testStat <- sqrt(n) * (mean(diff) - 0)/sd(diff) #test Statistic: 2.65

#we get the p-value by multiplying the probability of getting a test statistic 
# as large or larger than 2.65 for a Gossett's T distribution with n-1 dof by two, since it's a two sided test
2 * pt(abs(testStat), n-1, lower.tail = FALSE) # Since we're working with 48 dof, there's little diference from calculating a pnorm or a pt

## or using the R function
t.test(diff)

-----------------
# P-value for an exact single Binomial Test 

pbinom(10, 20, .1, lower.tail = FALSE)

In [1]: 7.088606e-07

#### 

binom.test(11, 20, .1, alternative="greater")

In [1]: Exact binomial test
In [2]: data:  11 and 20
In [3]: number of successes = 11, number of trials = 20, p-value = 7.089e-07
In [4]: alternative hypothesis: true probability of success is greater than 0.1
In [5]: 95 percent confidence interval:
In [6]:  0.3469314 1.0000000
In [7]: sample estimates:
In [8]: probability of success 
In [9]:                   0.55 

------------------
# Performing an Exact Fisher's test
dat <- matrix(c(4,1,2,3), 2)
fisher.test(dat, alternative="greater")

In [1]: Fishers Exact Test for Count Data
In [2]: data:  dat
In [3]: p-value = 0.2619
In [4]: alternative  hypothesis: true odds ratio is greater than 1
In [5]: 95 percent confidence interval:
In [6]:  0.3152217       Inf
In [7]: sample estimates:
In [8]: odds ratio 
In [9]:   4.918388 


-------------------
# Chi-squared value 

pchisq(8.96, 1, lower.tail = FALSE) 

In [1]: 0.002

# Performing a chi-squared test 
dat <- matrix(c(44, 77, 56, 43), 2)
chisq.test(dat)
chisq.test(dat, correct = FALSE)

In [1]: Pearsons Chi-squared test with Yates continuity correction

In [2]: data:  dat
In [3]: X-squared = 8.1667, df = 1, p-value = 0.004267


In [4]:	Pearsons Chi-squared test

In [5]: data:  dat
In [6]: X-squared = 8.963, df = 1, p-value = 0.002755

--------------------

# Performing a Generalised Chi-squared test
x <- matrix(c(7,7,2,3,
              2,8,3,7,
              1,5,4,9,
              2,8,9,14),4)
chisq.test(x)     

In [1]: Pearsons Chi-squared test

In [2]: data:  x
In [3]: X-squared = 16.955, df = 9, p-value = 0.04942

In [4]: Warning message:
In [5]: In chisq.test(x) : Chi-squared approximation may be incorrect

--------------------

# Performing a Cochran Mantel-Haenszel test to determine whether or not the odds ratio is one, given that it's common across all strata

dat <- array(c(11, 10, 25, 27,  16, 22,  4, 10,
               14,  7,  5, 12,   2,  1, 14, 16,
                6,  0, 11, 12,   1,  0, 10, 10,
                1,  1,  4,  8,   4,  6,  2,  1),
             c(2,2,8))
mantelhaen.test(dat, correct=FALSE)

In [1]: Mantel-Haenszel chi-squared test without continuity correction

In [2]: data:  dat
In [3]: Mantel-Haenszel X-squared = 6.3841, df = 1, p-value = 0.01151
In [4]: alternative hypothesis: true common odds ratio is not equal to 1
In [5]: 95 percent confidence interval:
In [6]: 1.177590 3.869174
In [7]: sample estimates:
In [8]: common odds ratio 
In [9]:         2.134549

--------------------

# Performing a Wilcoxon's non-parametric signed rank test to determine whether the median of the data is zero or not

diff <- c(.07, .07, .00, -.04,...)
wilcox.test(diff,exact=FALSE)

In [1]: Wilcoxon signed rank test with continuity correction

In [2]: data:  diff
In [3]: V = 5, p-value = 0.4142
In [4]: alternative hypothesis: true location is not equal to 0
