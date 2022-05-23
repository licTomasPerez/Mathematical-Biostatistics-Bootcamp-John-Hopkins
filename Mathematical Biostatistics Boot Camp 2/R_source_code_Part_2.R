#### Homework 1 - Exercise 1 
## In a  random sample of 100 patients at a clinic, you would like to test whether the mean RDI is 10 or more using a one sided 5\% type 1 error rate.
## The sample mean RDI was 12 with a standard deviation of 4. Do you reject the null hypothesis?

n <- 100
mu <- 12
mu0 <- 10 
sd <- 4
alpha <- .05

testStat <- (mu - mu0)/(sd/sqrt(n))
qtile <- qnorm(alpha, lower.tail = FALSE)

## We reject the null hypothesis if the test statistic is greater or equal than the quantile
reject <- testStat > qtile 
print(reject)

In [1]: TRUE

-------------

#### Homework 1 - Exercise 2
## A pharmaceutical company is interested in testing a potential blood pressure lowering medication. 
## Their first examination considers only subjects that received the medication at baseline then two weeks later. The data are as follows (SBP in mmHg)

# Two sided paired T-test for two data groups
group1 = c(140,138,150,148,135) # R-array of group 1-data points
group2 = c(132,135,151,146,130) # R-array of group 2-data points
diff <- group2-group1 # difference of the two data groups
n <- sum(!is.na(diff)) # number of pairs of data points
mu <- mean(diff) # mean of the difference of the data groups
standard_deviation <- sd(diff) # standard deviation of the data groups 
testStat <- sqrt(n) * (mean(diff) - 0)/sd(diff) 

Pvalue <- 2 * pt(abs(testStat), n-1, lower.tail = FALSE) 
Pvalue

In [1]: 0.08652278
------------

group1 = c(140,138,150,148,135) # R-array of group 1-data points
group2 = c(132,135,151,146,130) # R-array of group 2-data points
t.test(group2, group1, alternative="two.sided", paired=TRUE)
t.test(diff)

Paired t-test

In [1]: data:  group2 and group1
In [2]: t = -2.2616, df = 4, p-value = 0.08652
In [3]: alternative hypothesis: true difference in means is not equal to 0
In [4]: 95 percent confidence interval:
In [5]: -7.5739122  0.7739122
In [6]: sample estimates:
In [7]: mean of the differences 
In [8]:             -3.4 

#### 

# Two sided T test for a single data group (the difference) 

group1 = c(140,138,150,148,135) # R-array of group 1-data points
group2 = c(132,135,151,146,130) # R-array of group 2-data points
t.test(group2 - group1, alternative = "two.sided")

Paired t-test

In [1]: data:  group2 and group1
In [2]: t = -2.2616, df = 4, p-value = 0.08652
In [3]: alternative hypothesis: true difference in means is not equal to 0
In [4]: 95 percent confidence interval:
In [5]: -7.5739122  0.7739122
In [6]: sample estimates:
In [7]: mean of the differences 
In [8]:             -3.4 

####

# Note this is not equivalent to a one-sided two-sided test

group1 = c(140,138,150,148,135) # R-array of group 1-data points
group2 = c(132,135,151,146,130) # R-array of group 2-data points

In [1]: Paired t-test
data: group2 and group1
In [2]: t = -2.262, df = 4, In In [3]: p-value = 0.04326
In [3]: alternative hypothesis: In [4]: true difference in means is less than 0
In [5]: 95 percent confidence interval:
In [6]: -Inf -0.1951
In [7]: sample estimates:
In [8]: mean of the differences
In [9]:             -3.4

-------------

#### Homework 1 - Exercise 3
## A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. 
## For what values of $\mu_0$ would a test of $H_0: \mu = \mu_0$  fail to reject the null hypothesis in a two sided 5\% Students t-test?

n <- 9
mu0 <- 1100
sd <- 30

CI <- (c(-1,1) * qt(0.975,n-1) * sd/sqrt(n-1) + mu0)
CI

In [1]: 1075.541 1124.459

-------------

#### Homework 1 - Exercise 4
## To further test a hospital triage system, administrators selected 200 nights and randomly assigned a new triage system to be used on 100 nights 
## and a standard system on the remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a physician.
## The average MWT for the new system was 4 hours with a standard deviation of .5 hours while the average MWT for the old system was 6 hours with 
## a standard deviation of 2 hours. Test the hypothesis of a decrease in the mean MWT associated with the new treatment using a two sided 5\% Z test.
## Does it appear to be effective?

n_old <- n_new <- n_pairs <- 100
mu_old <- 4
sd_old <- .5 
n_new <- 100
mu_new <- 6
sd_new <- 2
test_stat <- mu_new - mu_old + c(-1, 1) * qnorm(0.975) * sqrt(sd_new^2/n_new + sd_old^2/n_old)
test_stat

In [1]: 1.595943 2.404057

-------------

#### Homework 1 - Exercise 5
## Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and 
## again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was -3 kg/m2 
## for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group 
## and 1.8 kg/m2 for the placebo group. Does the change in BMI over the two year period appear to differ between the treated and placebo groups? 
## Assuming normality of the underlying data and a common population variance, give the relevant 95\% t confidence interval.

n <- 9
mu_treated <- -3
sd_treated <- 1.5

mu_placebo <- 1
sd_placebo <- 1.8

Sp <- sqrt((n-1)/(n+n-2)*sd_treated + (1-(n-1)/(n+n-2))*sd_placebo)

(mu_treated - mu_placebo)/(Sp*sqrt(1/n + 1/n))

-------------

#### Homework 1 - Exercise 6
## Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline
## and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) 
## was -3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for 
## the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over the two year period appear to differ between the treated 
## and placebo groups?  Assuming normality of the underlying data and a common population variance, give the relevant 95\% t confidence interval.

n <- 9
mu_treated <- -3
sd_treated <- 1.5

mu_placebo <- 1
sd_placebo <- 1.8

Sp <- sqrt((n-1)/(n+n-2)*sd_treated + (1-(n-1)/(n+n-2))*sd_placebo)

CI <- (mu_treated-mu_placebo) + c(-1,1)*(qt(.975, 9+9-2) * Sp * sqrt(2/n))
CI

-------------

#### Homework 1 - Exercise 7
## Suppose that systolic blood pressures were taken on 16 oral contraceptive users and 16 controls at baseline and again then two years later. 
## The average difference from follow-up SBP to the baseline (followup - baseline) was 11 mmHg for oral contraceptive users and 4 mmHg for controls. 
## The corresponding standard deviations of the differences was 20 mmHg for OC users and 28 mmHg for controls.
## Assuming that blood pressure are normally distributed and an equal variance, give a P-value for a two sided hypothesis test that the change in BP 
## differs for OC users compared to controls.


n1 <- n2 <- 16
dif1 <- 11
dif2 <- 4
sd_dif1 <- 20
sd_dif2 <- 28

sp <- sqrt((sd_dif1^2+sd_dif2^2)/2)
test_stat <- (dif2 - dif1)/(sp * sqrt(1/n1 + 1/n2))
p_value <- 2 * pt(test_stat, df = (n1+n2-2))
p_value

In [1]: 0.4222096

-------------

#### Homework 1 - Exercise 8
## Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume loss of $.01~mm^3$. 
## Assume that the standard deviation of four year volume loss in this population is $.04~mm^3$. What would be the power of the study for a 
## 5\% one sided test versus a null hypothesis of no volume loss?

no_sim <- 100000 # number of simulation to perform 
n_dof <- 100 # number of degrees of freedom
sigma <- 0.04 # variance 
alpha <- .05 

mu0 <- .01 # RDI mean under the null hypothesis
mua <- 0 # RDI mean under the alternative hypothesis
z <- rnorm(no_sim) # rnorm is the R function that simulates random variables having a specified normal distribution
chisq <- rchisq(no_sim, df = n_dof - 1) # chi squared distribution
t_qt <- qt(.95, n_dof-1) # 95th quantile for the Gossett's T distribution
mean(z + sqrt(n_dof)*(mua-mu0)/sigma > 
    t_qt/sqrt(n_dof-1)*sqrt(chisq))
    
In [1]: 0

-------------

#### Homework 1 - Exercise 9
## The Daily Planet ran a recent story about Kryptonite poisoning in the water supply after a recent event in Metropolis.
## Their usual field reporter, Clark Kent, called in sick and so Lois Lane reported the stories. Researchers sampled 288 individuals and 
## found mean blood Kryptonite levels of 44, both measured in Lex Luthors per milliliter (LL/ml). They compared this to 288 sampled individuals 
## from Gotham city who had an average level of 42.04. Report the Pvalue for a two sided Z test of the relevant hypothesis. 
## Assume that the standard deviation is 12 for both groups.

P_value <- pnorm(-1.96, mean = 2, sd = 1) + pnorm(1.96, mean = 2, sd = 1)
P_value 

In [1]: 0.484084

------------- -------------------
------------- -------------------

#### Quiz 1 - Week 1
## A pharmaceutical company is interested in testing a potential blood pressure lowering medication. Their first examination considers only subjects that 
## received the medication at baseline then two weeks later. The data is given as follows (SBP in mmHg)
## Test the hypothesis that there was a mean reduction in blood pressure. Compare the difference between a paired and unpaired test for a two sided 5\% level test.

# Part A: one sided paired two-group ordinary test 
data_X_group1 <- c(140,138,150,148,135)
data_Y_group2 <- c(137,136,148,146,133)
alpha <- .05

diff <- data_X_group1 - data_Y_group2
n <- sum(!is.na(diff)) # number of pairs of data points
mu <- mean(diff) # mean of the difference of the data groups
std_error <- sd(diff) # standard deviation of the data groups 

testStat <- sqrt(n) * (mean(diff) - 0)/sd(diff) 

Pvalue <- 2 * pt(abs(testStat), n-1, lower.tail = FALSE) 

if(Pvalue > alpha/2){
    print("Reject the null hypothesis")
} else {
    print("Fail to reject the null hypothesis")
}

cat("Mean data group 1", mean(data_X_group1))
cat("Mean data group 2", mean(data_Y_group2))
cat("Mean difference", mean(diff))
cat("TS",testStat)
cat("P-value", Pvalue)

# Part B: one sided independent two-group ordinary test 

data_X_group1 <- c(140,138,150,148,135)
data_Y_group2 <- c(137,136,148,146,133)
alpha <- .05

mu1 <- mean(data_X_group1)
mu2 <- mean(data_Y_group2)
sd1 <- sd(data_X_group1)
sd2 <- sd(data_Y_group2)

n1 <- n2 <- 5 

sp <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)) # pooled variance
testStat <- (mu1 - mu2)/(sp * sqrt(1/n1+1/n2))

t_qt <- qt(1-alpha, n1+n2-2)

if(testStat > t_qt){
    print("Reject the null hypothesis")
} else {
    print("Fail to reject the null hypothesis")
}

cat("pooled variance", sp)
cat("test Statistic", testStat)
cat("95th T-quantile", t_qt)

In [1]: "Fail to reject the null hypothesis"
In [2]: pooled variance 6.545991
In [3]: test Statistic 0.5313948
In [4]: 95th T-quantile 1.859548
