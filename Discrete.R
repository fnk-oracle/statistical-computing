accept_reject_quad <- function(n) {
  target_pdf <- function(x) (3/8) * (4 - x^2) #p(x)
  proposal_pdf <- function(x) 1/4 # q(x)
  M <- 2 #chosen such that  M*q(x) >= p(x)
  
  samples <- numeric(n)
  i <- 1
  
  while(i <= n){
    x <- runif(1, -2, 2) # sample from proposal
    u <- runif(1)
    if (u< target_pdf(x)/(M * proposal_pdf(x)))
      samples[i] <- x
      i <- i+1
  }
  return(samples)
}

samples <- accept_reject_quad(1000)
print(length(samples))
hist(samples, probability=TRUE, col='lightblue', main="Accept-Reject: Quadratic Distribution")
