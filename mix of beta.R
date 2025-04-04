# Composition: Mixture of Beta Distributions
# We sample Beta(2,5), with probability 0.6 and Beta(,2) with probability 0.4

mixture_beta <- function(n){
  samples <- numeric(n)
  for (i in 1:n){
    if(runif(1)<0.6){
      samples[i] <- rbeta(1,2,5)
    }
    else{
      samples[i] <- rbeta(1,5,2)
    }
  }
  return(samples)
}

samples <- mixture_beta(3000)
hist(samples, probability = TRUE, col='lightyellow', main='Mixture of Beta Distributions')