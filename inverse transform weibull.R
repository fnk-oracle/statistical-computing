#Inverse Transform: Weibull Distribution
# Given F(x) = e ^ {-(x/lambda)^k}, for x=F^{-1}(u)
# x = lambda(-log(1-u))^{1/k}

inverse_weibull <- function(n,lambda, k){
  return(lambda*(-log(runif(n))^(1/k)))
}

samples <- inverse_weibull(5000, 2, 1.5)
hist(samples, probability = TRUE, col='lightgreen', main='Inverse Transform: Weibull')