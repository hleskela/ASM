library(mgcv)
library(SemiPar)

data <- read.table("hirsutism.dat", sep = "\t", header = TRUE)
attach(data)
# Trying different models explaining FGm12 with different explanatory variables

treatment.model <- spm(FGm12 ~ Treatment)
FGm0.model <- spm(FGm12 ~ FGm0)
FGm3.model <- spm(FGm12 ~ FGm3)
FGm6.model <- spm(FGm12 ~ FGm6)
SysPres.model <- spm(FGm12 ~ SysPres, omit.missing = TRUE)
DiaPres.model <- spm(FGm12 ~ DiaPres)
weight.model <- spm(FGm12 ~ weight)
height.model <- spm(FGm12 ~ height)
