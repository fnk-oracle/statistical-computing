#Monte-Carlo Estimation of pi Using Accpet-Reject 
# We estimate pi by simulating random points inside a unit square and counting how many fall inside a unit circle
#the probability of a point landing inside the circle is :
# P = (pi * r^2) / (4 * r^2)
# So pi is approximately 4 * (points inside the circle/ total points)

estimate_pi <- function(n){
  x <- runif(n, -1, 1) # Random X in [-1,1]
  y <- runif(n, -1, 1) # Random Y in [-1.1]
  
  inside <- (x^2 +y^2) <= 1 # check if inside circle
  pi_estimate <- 4*mean(inside) # compute pi estimate
  return(pi_estimate)
}

set.seed(123)
pi_est <- estimate_pi(10000)
cat("Estimated pi: ", pi_est, "\n")
