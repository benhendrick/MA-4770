# MA 4780 Homework 3
# March 2, 2016

# Problem 4.2
sketchMA2 <- function(theta1, theta2) {
  y <- ARMAacf(ma = c(-theta1,-theta2), lag.max = 20)
  plot(y, x = 0:20, 
       type = "h", 
       ylim = c(-1,1), 
       xlab = "k", 
       ylab = "Autocorrelation", 
       main = paste("Population ACF of an MA(2) model with coefficiens ", theta1, " and ", theta2))
  abline(h=0)
}

# Parts A, B, and C
sketchMA2(0.5,0.4)
sketchMA2(1.2,-0.7)
sketchMA2(-1,-0.6)

# Problem 4.5
sketchAR1 <- function(phi1, lags) {
  y <- ARMAacf(ar = phi1, lag.max = lags)	
  plot(y, x = 0:lags, 
       type = "h", 
       ylim = c(-1,1), 
       xlab = "k", 
       ylab = "Autocorrelation", 
       main = paste("Population ACF of an AR(2) model with coefficient ", phi1))
  abline(h=0)
}

# Parts A, B, C, and D
sketchAR1(0.6, 10)
sketchAR1(-0.6, 10)
sketchAR1(0.95, 20)
sketchAR1(0.3, 5)

# Problem 4.9
recursiveAR2 <- function(phi1, phi2, lags) {
  max.lag = lags
  rho = rep(0,max.lag)
  rho[1] = phi1/(1-phi2)
  rho[2] = (phi2*(1-phi2)+phi1^2)/(1-phi2)
  for (k in 3:max.lag) {
    rho[k] = phi1*rho[k-1]+phi2*rho[k-2]
  }
  plot(y=c(1,rho), x=0:max.lag, type='h', ylab='ACF', xlab='Lag', ylim=c(-1,+1), main = "Population ACF of an AR(2) model with coefficients 1 and -0.5")
  abline(h=0)
  
  results <- list()
  results$roots <- polyroot(c(1,phi1,phi2))
  results$class <- class(polyroot(c(1,phi1,phi2)))
  results$R <- sqrt(-phi2+0i)
  results$theta <- cospi(-(phi1 * sqrt(-phi2+01))/(2 *phi2))
  return(results)
}

# Parts A, B, C, D, E, and F
recursiveAR2(0.6,0.3,20)
recursiveAR2(-0.4,0.5,20)
recursiveAR2(1.2,-0.7,20)
recursiveAR2(-1,-0.6,20)
recursiveAR2(0.5,-0.9,20)
recursiveAR2(-0.5,-0.6,20)

# Problem 4.12
