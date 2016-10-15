library(sm)

attach(aircraft)
lgPower <- log(Power)
lgSpan <- log(Span)
lgLength <- log(Length)
lgWeight <- log(Weight)
lgSpeed <- log(Speed)
lgRange <- log(Range)

# Plots the log(weight) of an airplane as a resultant of model year, Yr
plot(Yr,lgWeight, main="Plot 1", sub="The log of the weights of planes vs the manufacturing year")

# Loess.smooth produces continuous values instead of the discrete ones we can use for years
#m = loess.smooth(Yr, lgWeight)

# Step a)
m = supsmu(Yr,lgWeight)
lines(m, col="red")

x <- unlist(m[1])
y <- unlist(m[2])

# Step b)
z <- 0
for (i in seq_along(Yr)) {
  index = which(x == Yr[i])
  z[i] <- log((lgWeight[i]-y[index])**2)
}

# Step c), calculate the log(variance) estimate
q <- supsmu(Yr,z)

# Step d), calculate sigma squared
unlistedSigmaEstimate <- unlist(q[2])
sigmaSquared <- 0
for (j in seq_along(unlistedSigmaEstimate)) {
  sigmaSquared[j] <- exp(unlistedSigmaEstimate[j])
}

# Calculating the estimated square error, e^(z(x)-q(x)) = E^2
e <- 0
qX <- unlist(q[1])
qY <- unlistedSigmaEstimate
for (k in seq_along(Yr)) {
  index = which(qX == Yr[k])
  #e[i] <- log((lgWeight[i]-y[index])**2)
  e[k] <- exp(z[k] - qY[index])
}

# Plotting the error squared againt the manufacturing year
plot(Yr,e,main="Plot 2", sub="The error squared of the model vs manufacturing year")
# Superimposing the estimated variance
unique_years <- unique(Yr)
lines(unique_years,sigmaSquared, col='red')
legend(x="topright", inset=.05, cex = 1, c("Variance"), lty=c(1,1), lwd=c(2,2), col=c("red"), bg="grey96")

# Plotting the regression line with 95 % confidence interval
plot(Yr, lgWeight,xlab="Yr", ylab="lgWeight",main="Plot 3", sub="The regression line with a 95% confidence interval")
lines(unique_years, unlist(m[2])+(1.96*sigmaSquared),col='blue',lwd=2)
lines(unique_years, unlist(m[2])-(1.96*sigmaSquared), col='red',lwd=2)
lines(m, col='green',lwd=2)
legend(x=12,y=13, inset=.05, cex = 1, c("Lower 5%","Model","Upper 5%"), lty=c(1,1), lwd=c(2,2), col=c("red","green","blue"), bg="grey96")