library("sm")
source("locpolreg.R")

# Input: Vectors x and y; a vector h.v of candidate values for h.
PMSE.CV <- function(x, y, h.v){
  # 
  # x <- c(1,2,3,4,5)
  # y <- c(4,8,6,3,2)
  # h.v <- c(0.3,1)
  
  N <- length(x)
  validation.x <- 0
  validation.y <- 0
  h.error <- rep(0, length(h.v))
  for(h in 1:length(h.v)){
    for (i in 1:N) {
      validation.x <- x[i]
      validation.y <- y[i]
      m <- loess(x[-i] ~ y[-i], span=h)
      predicted.y <- predict(m,validation.x)
      residual.y <- validation.y - predicted.y
      h.error[h] <- h.error[h] + residual.y^2
    }
    h.error[h] <- sqrt(h.error[h])
  }
  minimum.index <- match(min(h.error), h.error)
  return(h.v[minimum.index])
}