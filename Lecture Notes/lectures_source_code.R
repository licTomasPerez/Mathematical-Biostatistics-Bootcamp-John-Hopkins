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
