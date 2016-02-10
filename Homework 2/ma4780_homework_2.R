# Problem 3.7 
library(TSA)
data("winnebago")

## Part A
plot(winnebago,
     type="o",
     ylab="Unit Sales",
     main="Monthly Unit Sales over Time")
model <- lm(winnebago~time(winnebago))
abline(model)

## Part B
plot(rstudent(model),
     type = "o",
     ylab = "Standard Residuals",
     main = "Monthly Unit Sales")

## Part C
winnLog <- log(winnebago)
plot(winnLog, 
     type="o",
     ylab = "Unit Sales (Log Scale)", 
     main = "Monthly Unit Sales")
logModel <- lm(winnLog~time(winnLog))
abline(logModel)

## Part D
plot(rstudent(model),
     type = "o",
     ylab = "Standard Residuals",
     main = "Monthly Unit Sales")

## Part E

month <- season(winnebago)
winCombo <- lm(winnLog~month + time(winnLog))
summary(winCombo)

## Part F
plot(rstudent(winCombo), 
     type = "o", 
     ylab = "Standard Residuals", 
     main ="Monthly Unit Sales")


# Problem 3.8
data(retail)

## Part A
plot(retail,
     type = "l",
     ylab = "Billions of Pounds",
     main = "Retail Sales over Time")
points(y=retail,
       x=time(retail), 
       pch=as.vector(season(retail)))

## Part B
retail.lm <- lm(retail~season(retail) + time(retail))
summary(retail.lm)

## Part C
plot(rstudent(retail.lm),
     type = "l",
     ylab = "Standardized Residuals",
     main = "Retail Sales")
points(y=rstudent(retail.lm),
       x=as.vector(time(rstudent(retail.lm))), 
       pch=as.vector(season(retail)))


# Problem 3.9
data(prescrip)

## Part A
plot(prescrip,
     type = "l",
     ylab = "Presciption Costs",
     main = "Presciption Costs over Time")
points(y = prescrip,
       x = time(prescrip),
       pch = as.vector(season(prescrip)))

## Part B
percPrecrip <- na.omit(100*(prescrip - zlag(prescrip))/zlag(prescrip))
plot(percPrecrip,
     type = "l",
     ylab = "Percent Change of Prescription Costs",
     main = "Percent Change of Prescription Costs over Time")
points(y = percPrecrip,
       x = time(percPrecrip),
       pch = as.vector(season(percPrecrip)))

## Part C
prescrip.lm <- lm(percPrecrip ~ harmonic(percPrecrip))
summary(prescrip.lm)

## Part D
plot(rstudent(prescrip.lm),
     type = "l",
     ylab = "Standardized Residuals",
     main = "Presciption Costs")
points(y=rstudent(prescrip.lm),
       x=as.vector(time(rstudent(prescrip.lm))), 
       pch=as.vector(season(prescrip)))

# Problem 3.13
data("winnebago")

## Part A
winnebago.lm <- lm(winnebago~season(log(winnebago)) + time(log(winnebago)))

## Part B
runs(rstudent(winnebago.lm))

## Part C
acf(rstudent(winnebago.lm))

## Part D
hist(rstudent(winnebago.lm),
     xlab = "Standardized Residuals",
     main = "Historgram of Standardized Residuals")
qqnorm(rstudent(winnebago.lm))
qqline(rstudent(winnebago.lm))

# Problem 3.14
data("retail")

## Part A
retail.lm <- lm(retail~season(retail) + time(retail))

## Part B
runs(rstudent(retail.lm))

## Part C
acf(rstudent(retail.lm))

## Part D
hist(rstudent(retail.lm),
     xlab = "Standardized Residuals",
     main = "Historgram of Standardized Residuals")
qqnorm(rstudent(retail.lm))
qqline(rstudent(retail.lm))


# Problem 3.15
data("prescrip")

## Part A
percPrecrip <- na.omit(100*(prescrip - zlag(prescrip))/zlag(prescrip))
prescrip.lm <- lm(percPrecrip ~ harmonic(percPrecrip))

## Part B
runs(rstudent(prescrip.lm))

## Part C
acf(rstudent(prescrip.lm))

## Part D
hist(rstudent(prescrip.lm),
     xlab = "Standardized Residuals",
     main = "Historgram of Standardized Residuals")
qqnorm(rstudent(prescrip.lm))
qqline(rstudent(prescrip.lm))