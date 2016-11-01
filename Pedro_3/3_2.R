source("PMSE.CV.R")
load("boston.Rdata")
names(boston.c)[13]<-'ROOM'

attach(boston.c)

x <- LSTAT
y <- ROOM
h.v <- exp( seq(from=log(.5), to = log(15), length=12))
