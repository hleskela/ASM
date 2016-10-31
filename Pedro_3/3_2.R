library("sm")

# Input: Vectors x and y; a vector h.v of candidate values for h.
PMSE.CV <- function(x, y, h.v){
  N <- length(x)
  validation.x <- 0
  validation.y <- 0
  for (i in 1:N) {
    validation.x <- x[i]
    validation.y <- y[i]
    
  }
  
}