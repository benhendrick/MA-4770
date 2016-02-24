# MA 4780 Take Home Exam 1
# Benjamin Hendrick
# Started 2/24/2016

# Question 1
# Part A
## Part i
y <- ARMAacf(ma = c(-0.8,0.1), lag.max = 20)	
plot(y, x = 0:20, 
     type = "h", 
     ylim = c(-1,1), 
     xlab = "k", 
     ylab = "Autocorrelation", 
     main = "Population ACF of an MA(2) model with coefficients 0.8 and -0.1")
abline(h=0)

## Part ii
library(TSA)
set.seed(12345)
n <- 100
e <- rnorm(n+1)
y <- ts(e[3:(n+2)]+0.8*e[2:(n+1)]-0.1*e[1:n])
fm <- lm(y~1)
acf(resid(fm),main ="Sample ACF plot for the MA(2) model with coefficients 0.8 and -0.1")

## Part iii
set.seed(23456)
n <- 100
e <- rnorm(n+1)
y <- ts(e[3:(n+2)]+0.8*e[2:(n+1)]-0.1*e[1:n])
fm <- lm(y~1)
acf(resid(fm),main ="Sample ACF plot for the MA(2) model with coefficients 0.8 and -0.1")

set.seed(34567)
n <- 100
e <- rnorm(n+1)
y <- ts(e[3:(n+2)]+0.8*e[2:(n+1)]-0.1*e[1:n])
fm <- lm(y~1)
acf(resid(fm),main ="Sample ACF plot for the MA(2) model with coefficients 0.8 and -0.1")

set.seed(45678)
n <- 100
e <- rnorm(n+1)
y <- ts(e[3:(n+2)]+0.8*e[2:(n+1)]-0.1*e[1:n])
fm <- lm(y~1)
acf(resid(fm),main ="Sample ACF plot for the MA(2) model with coefficients 0.8 and -0.1")

set.seed(56789)
n <- 100
e <- rnorm(n+1)
y <- ts(e[3:(n+2)]+0.8*e[2:(n+1)]-0.1*e[1:n])
fm <- lm(y~1)
acf(resid(fm),main ="Sample ACF plot for the MA(2) model with coefficients 0.8 and -0.1")

# Part B
