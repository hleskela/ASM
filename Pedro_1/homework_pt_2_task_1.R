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
#regressionData = loess.smooth(Yr, lgWeight)

# Step a)
regressionData = supsmu(Yr,lgWeight)
lines(regressionData, col="red")

x <- unlist(regressionData[1])
y <- unlist(regressionData[2])

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
