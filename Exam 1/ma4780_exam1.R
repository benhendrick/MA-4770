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
for (i in 1:5){
  set.seed(i)
  n <- 100
  e <- rnorm(n+1)
  y <- ts(e[3:(n+2)]+0.8*e[2:(n+1)]-0.1*e[1:n])
  fm <- lm(y~1)
  acf(resid(fm),main ="Sample ACF plot for the MA(2) model with coefficients 0.8 and -0.1")
  
}

# Part B
## Part i
y <- ARMAacf(ar = 0.9, lag.max = 20)	
plot(y, x = 0:20, 
     type = "h", 
     ylim = c(-1,1), 
     xlab = "k", 
     ylab = "Autocorrelation", 
     main = "Population ACF of an AR(1) model with coefficient 0.9")
abline(h=0)

## Part ii
set.seed(2190)
n = 100
e = rnorm(n+1)
phi1 = 0.9
y = rep(0,n); y[1] = e[1];
for (i in 2:n){ y[i] = phi1*y[i-1]+e[i]}
acf(y,main="Sample ACF of an AR(1) model with coefficient 0.9")

## Part iii
for (i in 1:5){
  set.seed(i)
  n = 100
  e = rnorm(n+1)
  phi1 = 0.9
  y = rep(0,n); y[1] = e[1];
  for (i in 2:n){ y[i] = phi1*y[i-1]+e[i]}
  acf(y,main="Sample ACF of an AR(1) model with coefficient 0.9")
}

# Problem 2
library(TSA)
#filePath <- "//homedir.mtu.edu/home/Desktop/Exam1-permit.csv"
filePath <- "~/GitHub/MA-4780/Exam 1/Exam1-permit.csv" # Mac
permit <- read.csv(filePath, header=FALSE)
permit <- ts(permit$V2, start = c(1991,1), end = c(2005,12), frequency = 12)

# Part A
plot(permit,
     type="l",
     ylab="New Residential Construction",
     xlab="Time",
     main="New Residential Constrution over Time")
points(y = permit,
       x = time(permit),
       pch = as.vector(season(permit)))

# Part B
permit.lm <- lm(permit~time(permit))
permit.resid <- resid(permit.lm)

# Part C
plot(permit.resid,
     type = "o",
     ylab = "Raw Residuals",
     main = "REsiduals of New Residential Construction over Time")

# Part D
permit.resid.lm <- lm(permit.resid~time(permit.resid))
plot(permit.resid.lm, which=1)

## Parts i, ii, and iii
summary(permit.resid.lm)

# Part E
## Part i
mean(fitted(permit.resid.lm))

## Part ii
runs(rstudent(permit.resid.lm))[1]

## Part iii
plot(fitted(permit.resid.lm),
     type = "o",
     xlab = "Index",
     ylab = "Fitted Residuals",
     main = "Plot of Fitted Residuals")

## Part iv
plot(permit.resid.lm, which=2)
hist(fitted(permit.resid.lm),
     xlab = "Fitted Linear Error Process",
     main = "Histogram of Fitted Linear Error Process")


# Problem 3
#filePath <- "//homedir.mtu.edu/home/Desktop/Exam1-VMT.csv"
filePath <- "~/GitHub/MA-4780/Exam 1/Exam1-VMT.csv" # Mac
vmt <- read.csv(filePath, header=FALSE)
vmt <- ts(vmt, start = c(2000,1), end = c(2015,12), frequency = 12)

# Part A
plot(vmt,
     type="l",
     ylab="Vehicle Miles Traveled",
     xlab="Time",
     main="Vehicle Miles Traveled over Time")
points(y = vmt,
       x = time(vmt),
       pch = as.vector(season(vmt)))

# Part B
vmt.coslm <- lm(vmt~harmonic(vmt,1))
vmt.resid <- resid(vmt.coslm)

# Part C
plot(vmt.resid,
     type = "o",
     ylab = "Raw Residuals",
     main = "Residuals of Vehicle Miles Traveled over Time")

# Part D
vmt.resid.coslm <- lm(vmt.resid~poly(vmt.resid,3))
plot(vmt.resid.coslm, which=1)

## Parts i and ii
summary(vmt.resid.coslm)

# Part E
## Part i
mean(fitted(vmt.resid.coslm))

## Part ii
runs(rstudent(vmt.resid.coslm))[1]

## Part iii
plot(fitted(vmt.resid.coslm),
     type = "o",
     xlab = "Index",
     ylab = "Fitted Residuals",
     main = "Plot of Fitted Residuals")

## Part iv
plot(vmt.resid.coslm, which=2)
hist(fitted(vmt.resid.coslm),
     xlab = "Fitted Linear Error Process",
     main = "Histogram of Fitted Linear Error Process")



