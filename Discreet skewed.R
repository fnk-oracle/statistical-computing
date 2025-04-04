#2 Accept- Reject: Skewed Distribution with Gaussian Proposal 
#Given: f(x): C e^{-x^2}(1+x), we choose q(x) = N(0,1 ) since it approximates well on f(x)
#The max ratio M is determined empirically as M=2

accept_reject_skewed <- function(n){
  target_pdf <- function(x) exp(-x^2) * (1+x)
  proposal_pdf <- function(x) dnorm(x, mean=0, sd=1)
  M <-2 #Empirically determined

    samples <- numeric(n)
    i <- 1
    while(i<=n){
      x <- rnorm(1, mean=0, sd=1)
      u <- runif(1)
      if(u< target_pdf(x) / (M * proposal_pdf(x))){
        samples[i} <- x
      i <- i+1
      }
    }
return(samples)
}
samples <- accept_reject_skewed(5000)
hist(samples, probability = TRUE, col = 'lightcoral', main='Accept-Reject: Skewed Distribution')
