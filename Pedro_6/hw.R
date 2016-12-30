library(mgcv)
library(SemiPar)

data <- read.table("hirsutism.dat", sep = "\t", header = TRUE)
attach(data)

# Data exploration
# Four outliers for weight that shifts the mean to the upper end of the interval, giving the histogram
# more of a poisson distribution look than normal distribution
mean(weight, na.rm = TRUE)
median(weight, na.rm = TRUE)
boxplot(weight)
hist(weight)
plot(weight)

# But only one outlier for height, and a more centered box plot.
# Also, the box is a bit smaller than the weight box, meaning that it is a sharper unimodal peak.
# It appears to be normally distributed around a mean, as it should be.
mean(height, na.rm = TRUE)
median(height, na.rm = TRUE)
boxplot(height)
hist(height)


# Just out of interest, let's investigate the heavily critizised but still indicative measurement bmi
bmi <- weight/(height*height)
bmi.mean <- mean(bmi, na.rm = TRUE)
bmi.median <- median(bmi, na.rm = TRUE)

# We can see that there's a slight tendancy to being overweight in the test subjects, which
# is probably a better factor than just looking at the height or weight. 
# Trying different models explaining FGm12 with different explanatory variables

treatment.model <- spm(FGm12 ~ Treatment)
plot(treatment.model)
summary(treatment.model)
FGm0.model <- spm(FGm12 ~ f(FGm0))
plot(FGm0.model)
summary(FGm0.model)
SysPres.model <- spm(FGm12 ~ f(SysPres), omit.missing = TRUE)
plot(SysPres.model)
summary(SysPres.model)
DiaPres.model <- spm(FGm12 ~ f(DiaPres), omit.missing = TRUE)
plot(DiaPres.model)
summary(DiaPres.model)
weight.model <- spm(FGm12 ~ f(weight), omit.missing = TRUE)
plot(weight.model)
summary(weight.model)
height.model <- spm(FGm12 ~ f(height), omit.missing = TRUE)
plot(height.model)
summary(height.model)

bmi.model <- spm(FGm12 ~ f(bmi), omit.missing = TRUE)
plot(bmi.model)
summary(bmi.model)


# Since anova() doesn't work for spm, and the output of summary is very limited, we switch to gam()
# At least the plots include confidence bounds for univariate spline functions, cite:
# http://www.biostat.umn.edu/~hodges/RPLMBook/Manuscripts/SalkowskiClassProject.pdf

library(combinat)

params <- c("FGm0", "height", "weight", "DiaPres", "SysPres")
perm.1 <- combn(params,1)
perm.2 <- combn(params,2)
perm.3 <- combn(params,3)
perm.4 <- combn(params,4)
# eval(parse(text= "FGm0"))
j <-1
models <- list()
r2 <- list()
for (i in perm.1) {
  models[j] <- gam(FGm12 ~ Treatment + eval(parse(text = i)), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + eval(parse(text = i)), data=data))$r.sq
  j <- j+1
}

for( i in 1:dim(perm.2)[2]){
  models[j] <- gam(FGm12 ~ Treatment + eval(parse(text = perm.2[,i][1])) + eval(parse(text = perm.2[,i][2])), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + eval(parse(text = perm.2[,i][1])) + eval(parse(text = perm.2[,i][2])), data=data))$r.sq
  j <- j+1
}

for( i in 1:dim(perm.3)[2]){
  models[j] <- gam(FGm12 ~ Treatment + eval(parse(text = perm.3[,i][1])) + eval(parse(text = perm.3[,i][2])) + eval(parse(text = perm.3[,i][3])), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + eval(parse(text = perm.3[,i][1])) + eval(parse(text = perm.3[,i][2])) + eval(parse(text = perm.3[,i][3])), data=data))$r.sq
  j <- j+1
}

for( i in 1:dim(perm.4)[2]){
  models[j] <- gam(FGm12 ~ Treatment + eval(parse(text = perm.4[,i][1])) + eval(parse(text = perm.4[,i][2])) + eval(parse(text = perm.4[,i][3])) + eval(parse(text = perm.4[,i][4])), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + eval(parse(text = perm.4[,i][1])) + eval(parse(text = perm.4[,i][2])) + eval(parse(text = perm.4[,i][3])) + eval(parse(text = perm.4[,i][4])), data=data))$r.sq
  j <- j+1
}

# Doing it again with s()

for (i in perm.1) {
  models[j] <- gam(FGm12 ~ Treatment + s(eval(parse(text = i))), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + s(eval(parse(text = i))), data=data))$r.sq
  j <- j+1
}

for( i in 1:dim(perm.2)[2]){
  models[j] <- gam(FGm12 ~ Treatment + s(eval(parse(text = perm.2[,i][1]))) + s(eval(parse(text = perm.2[,i][2]))), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + s(eval(parse(text = perm.2[,i][1]))) + s(eval(parse(text = perm.2[,i][2]))), data=data))$r.sq
  j <- j+1
}

for( i in 1:dim(perm.3)[2]){
  models[j] <- gam(FGm12 ~ Treatment + s(eval(parse(text = perm.3[,i][1]))) + s(eval(parse(text = perm.3[,i][2]))) + s(eval(parse(text = perm.3[,i][3]))), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + s(eval(parse(text = perm.3[,i][1]))) + s(eval(parse(text = perm.3[,i][2]))) + s(eval(parse(text = perm.3[,i][3]))), data=data))$r.sq
  j <- j+1
}

for( i in 1:dim(perm.4)[2]){
  models[j] <- gam(FGm12 ~ Treatment + s(eval(parse(text = perm.4[,i][1]))) + s(eval(parse(text = perm.4[,i][2]))) + s(eval(parse(text = perm.4[,i][3]))) + s(eval(parse(text = perm.4[,i][4]))), data=data)
  r2[j] <- summary(gam(FGm12 ~ Treatment + s(eval(parse(text = perm.4[,i][1]))) + s(eval(parse(text = perm.4[,i][2]))) + s(eval(parse(text = perm.4[,i][3]))) + s(eval(parse(text = perm.4[,i][4]))), data=data))$r.sq
  j <- j+1
}

best.r2 <- max(as.numeric(r2))
index.of.best <- match(best.r2, r2)
# the 57th model has the best r2, i.e. Treatment + s(height) + s(weight) + s(DiaPres) + s(SysPres)
plot(as.numeric(r2))
# We can see that the models explain more and more the more complexity we add to them. Without test
# data it is hard to say if they overfit, but we can see that using s() for the splining gives a
# better result in general 