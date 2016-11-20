# One way anova for the fat
#setwd("~/Documents/UPC/ASM/Marta_2")
require(ggplot2)
dat = read.csv("Fat.csv")

# Four groups with their respective blood fat values. Does the mean differ? Let's do ANOVA to find out!
plot(dat)

# When we plot the data we can see that group one seem to have the biggest variance within the group,
# while 2 and 4 are much more compact. 3 is also quite compact, but has what appears to be an outlier.
# Let's plot the boxplots to find out
dat$Fat = factor(dat$Fat,labels = c("Group 1", "Group 2", "Group 3", "Group 4"))

ggplot(dat, aes(x = Fat, y = Absorbedgr)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Group") +
  ylab("Blood fat level")

# Create the linear model which we are to investigate
fat.mod1 = lm(Absorbedgr ~ Fat, data = dat)

# The summary will give us descriptive statistics regarding the support for H0
summary(fat.mod1)


# The Q Q plot should be linear, and the residual vs fitted should not expand like a cone
plot(fat.mod1)
