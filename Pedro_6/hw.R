library(mgcv)
library(SemiPar)

data <- read.table("hirsutism.dat", sep = "\t", header = TRUE)
attach(data)

# Data exploration
# Four outliers for weight that shifts the mean to the upper end of the interval
mean(weight, na.rm = TRUE)
median(weight, na.rm = TRUE)
boxplot(weight)
hist(weight)

# But only one outlier for height, and a more centered box
# Also, the box is a bit smaller than the weight box, meaning that it is a sharper 
mean(height, na.rm = TRUE)
median(height, na.rm = TRUE)
boxplot(height)
hist(height)



bmi.mean <- mean(weight/(height*height), na.rm = TRUE)
bmi.median <- median(weight/(height*height), na.rm = TRUE)

# We can see that there's a slight tendancy to being overweight in the test subjects, which
# is probably a better factor than just looking at the height or weight. 
# Trying different models explaining FGm12 with different explanatory variables

treatment.model <- spm(FGm12 ~ Treatment)
plot(treatment.model)
FGm0.model <- spm(FGm12 ~ FGm0)
plot(FGm0.model)
SysPres.model <- spm(FGm12 ~ SysPres, omit.missing = TRUE)
DiaPres.model <- spm(FGm12 ~ DiaPres, omit.missing = TRUE)
weight.model <- spm(FGm12 ~ weight, omit.missing = TRUE)
height.model <- spm(FGm12 ~ height, omit.missing = TRUE)

# Doesn't work for spm
#anova(treatment.model, FGm0.model, test = "F")
