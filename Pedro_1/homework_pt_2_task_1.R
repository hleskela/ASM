library(sm)

attach(aircraft)
lgPower <- log(Power)
lgSpan <- log(Span)
lgLength <- log(Length)
lgWeight <- log(Weight)
lgSpeed <- log(Speed)
lgRange <- log(Range)

# Plots the log(weight) of an airplane as a resultant of model year, Yr
plot(Yr,lgWeight)

#Loess.smooth produces continuous values instead of the discrete ones we can use for years
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
plot(Yr,e)
# Superimposing the estimated variance
unique_years <- unique(Yr)
lines(unique_years,sigmaSquared, col='red')

# Plotting the regression line with 95 % confidence interval
plot(m, ylim=c(0.0, 15))
lines(unique_years, unlist(m[2])+(1.96*sigmaSquared),col='blue')
lines(unique_years, unlist(m[2])-(1.96*sigmaSquared), col='red')
#lines(m, col='green')
