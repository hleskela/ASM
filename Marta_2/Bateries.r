# two way anova for the battery dataset
#setwd("~/Documents/UPC/ASM/Marta_2")
require(ggplot2)
dat = read.csv("Bateries.csv")

# Three different materials at three different temperatures. Below plots do not give as much information
# as the one way anova plot, but temp vs duration and material vs duration gives some hints.
plot(dat)

duration.df = data.frame(
  Material = dat$Material,
  Temperature = dat$Temp,
  Duration = dat$Duration
)

# Below plots the data using a dot plot to investigate if we see any visual trends between the different 
# material types
ggplot(duration.df, aes(Duration, Temperature, colour = Material)) + geom_point()

# I think that we can see a trend where material 3 seem to be the better choice. This is clear for 125
# and 75 degrees, but not as obvious for 15 degrees. The first material seem to be performing the worst at
# 75 and 125, and have the largest variance at 15

# Create the model which we are to investigate
duration.mod1 = aov(Duration ~ Temperature*Material, data = duration.df)

# The summary will give us descriptive statistics regarding the support for H0
summary(duration.mod1)

# 
ggplot(duration.res, aes(M1.Fit, M1.Resid, colour = Material)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals")
