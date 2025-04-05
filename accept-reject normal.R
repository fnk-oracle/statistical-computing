#Picking Numbers from a bell-shaped curve(Normal Distribution), but we can only pick numbers randomly from a box

accept_reject_normal <- function(n){
  samples <- numeric(n)
  i <- 1
  while (i<=n){
    x <- runif(1,-3,3) # pick a random number from a box (-3,3)
    u <- runif(1)
    
    if (u < dnorm(x)/0.4){ #Accept if it fits the curve
      samples[i] <- x
    } 
  }
  return(samples)
}

samples <- accept_reject_normal(10000)
hist(samples, probability=TRUE, COL='skyblue', main='Accept-Reject: Normal Distribution')
