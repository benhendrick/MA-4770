# MA 4780 Homework 4

# 5.1

# 5.12
library(TSA)
data("SP")

## Part A
plot(SP, 
     ylab = "Stock Price ($)", 
     main = "Standard & Poor's Compositive Index")

## Part B
SP.ln <- log(SP)
plot(SP.ln,
     ylab = "Natuaral Log Stock Price ($)",
     main = "Standard & Poor's Compositive Index")

## Part C
SP.rel <- diff(SP)/SP
plot(SP.rel,
     ylab = "Relative Change Stock Price ($)",
     main = "Standard & Poor's Compositive Index")
SP.diff.log <- diff(log(SP))
plot(SP.diff.log,
     ylab = "Difference Log Stock Price ($)",
     main = "Standards & Poor's Compositive Index")

## Part D
library(tseries)
acf(SP)
adf.test(SP)
pp.test(SP)

acf(SP.ln)
adf.test(SP.ln)
pp.test(SP.ln)

acf(SP.rel)
adf.test(SP.rel)
pp.test(SP.rel)

acf(SP.diff.log)
adf.test(SP.diff.log)
pp.test(SP.diff.log)


# 6.12
# 6.13

# 6.26
## Part A

## Part B

series <- arima.sim(n=48, list(ma=-0.5))
acf(series)

## Part C
theta <- 0.5
phikk <- rep(NA,10)
for(i in 1:10) {
  phikk[i] <- -(theta^i)*(1-theta^2)/(1-theta^(2*(i+1)))
}
plot(phikk, type ="h",
     main = "Theoretical Partial Autocorrelation",
     xlab = "Lag",
     ylab = "MA(1)")
abline(h=0)

## Part D
pacf(series)


# Exercise 6.31
series <- arima.sim(n=60, list(order = c(0,1,1), ma = -0.8))[-1]

## Part A
adf.test(series, k = 0)

## Part B
adf.test(series)

## Part C
adf.test(diff(series),k=0)
adf.test(diff(series))

# Exercise 6.36
data("robot")

## Part A
plot(robot, main = "Robot Maneuvers", ylab = "Distance (in)")

## Part B
acf(robot)
pacf(robot)

## Part C
eacf(robot)

## Part D
library(forecast)
auto.arima(robot)
