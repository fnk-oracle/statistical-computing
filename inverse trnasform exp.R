#Inverse Transform: Exponential Distribution
# Given CDF F(x) = 1- e^(-2x), solved for x = F^(-1)(U):
# X= -log(1-u)/2 (which simplifies to -log(u)/2, since u~U(0,1))

generate_exp <- function(n, lambda){
  return(-log(runif(n))/lambda)
}

samples <- generate_exp(1000,2)
hist(samples,probability = TRUE, col='lightyellow', main='Inverse Transform: Exponential')