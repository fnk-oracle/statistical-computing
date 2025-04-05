# MCMC Samplings for Bayesian Inference (Metropolis Hastings)
# Bayesian inference requires samplings from a posterior distributions
# If direct sampling is hard, we use MCMC(Markov Chain Monte Carlo)
# For bayesian estimation of mean theta, in Normal Distribution we will assume:
# Prior: theta~N(0,1)
# Likelihood: Xi~N(theta,1)
# Posterior: p(theta|X) directly proportional to p(X|theta)p(theta)

metropolis <- function(n,X,sigma){
  theta <- numeric(n)
  theta[1] <- 0 #start at 0
  
  for(i in 2:n){
    proposal <- rnorm(1, theta[i-1],sigma)
    likelihood_ratio <- prod(dnorm(X, mean=proposal,sd=1)/dnorm(X,mean=theta[i-1],sd=1))
    prior_ratio <- dnorm(proposal,mean=0, sd=1)/dnorm(theta[i-1],mean=0,sd=1)
    acceptance_prob <- min(1, likelihood_ratio * prior_ratio)
    
    if(runif(1) < acceptance_prob){
      theta[i] <- proposal
    }else{
      theta[i] <- theta[i-1]
    }
  }
  return(theta)
}

set.seed(123)
X <- rnorm(100, mean=2, sd=1)
theta_samples <- metropolis(10000, X, 0.5)

hist(theta_samples, probability=TRUE, col="lightblue", main="Posterior Distribution of theta")
