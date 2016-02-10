# Problem 3.7 
library(TSA)
data("winnebago")

## Part A
plot(winnebago,type="o",ylab="Unit Sales",main="Monthly Unit Sales over Time")

## Part B
model <- lm(winnebago~time(winnebago))
abline(model)

## Part C
winnLog <- log(winnebago)
plot(winnLog, type="o",ylab = "Unit Sales", main = "Monthly Unit Sales over Time")

## Part D
logModel <- lm(winnLog~time(winnLog))
abline(logModel)

## Part E

## Part F


# Problem 3.8

