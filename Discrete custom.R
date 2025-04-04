#Accept-Reject: Custom Power Distribution
#Given: g(x): (5/2)* x^4 for 0<=x<=1, we will use  q(x) = U(0,1), so q(x)=1
#The maximum of g(x)/(M*Q(x)) occurs at x=1, givinf M=2

gx <- function(x) (5/2)* x^4
accept_reject_custom <- function(x){
  samples <- numeric(n)
  i <- 1
  while(i<=n){
    x <- runif(1,0,1)
    u <- runif(1)
    if(u<gx(x)/2){
      samples[i] <- x
      i <- i+1
    }
  }
  return(samples)
}

samples <- accept_reject_custom(2000)
hist(samples, probability = TRUE, col='lightgreen', main='Accept-Reject: Custom Power')
