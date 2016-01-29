# 1.4
chiSq <- as.ts(rchisq(48, 2, ncp = 0))
plot(chiSq,
     xlab = "Time (t)",
     ylab = "Y",
     main = "Random Chi Squared Distribution")

# 1.5
t <- as.ts(rt(48, 5, ncp = 0))
plot(t,
     xlab = "Time (t)",
     ylab = "Y",
     main = "Random t-Distribution")

# 1.6
install.packages("TSA","mgcv")
library(TSA)
data("tempdub")
plot(tempdub, type='l', ylab='Sales', main = "Dubuque Temperatures over Time")
points(y=tempdub,x=time(tempdub),
       pch=as.vector(season(tempdub)))
