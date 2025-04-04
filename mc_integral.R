#Monte Carlo Integration with Accpet-Reject Sampling
#Using the accept-reject method to estimate  I = ∫_0^2 e^{-x^2} dx
# We approximate using I ~ mean(accepted x) * (b - a)

mc_integral <- function(n) {
  f <- function(x) exp(-x^2)
  samples <- numeric(n)
  
  for (i in 1:n){
    x <- runif(1, 0, 2)
    if(runif(1)<f(x)/1){
      samples[i] <- x
    }
  }
  return(mean(samples)*2) # Approximate Integral
}

estimated_I <- mc_integral(10000)
true_I <- integrate(function(x) exp(-x^2), 0, 2)$value
cat("Estimated Integral:", estimated_I, "\n True Inegral:", true_I)
