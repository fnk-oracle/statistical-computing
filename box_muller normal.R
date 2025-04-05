# Now lets do Box-Muller method 
#just imagine it like you want to draw a pair of marbles,
# but you want them to follow a special pattern.,Instead of rejecting some marbles,
# you magically transform random numbers into something useful
# Trick is : 1- Pick 2 random number 
#Use a math transformation,
#You get 2 marbles that follow the bell shaped curve(normal distribution)

box_muller <- function(n){
  u1 <- runif(n/2) # First random number (from 0 to 1)
  u2 <- runif(n/2) # Second random number (from 0 to 1)
  
  z1 <- sqrt(-2 * log(u1) * cos(2* pi *u2)) # first transformed number
  z2 <- sqrt(-2 * log(u1) * sin(2 * pi * u2)) #second transformed number
  
  return(c(z1,z2)) # combine both sets
}

samples <- box_muller(10000)
hist(samples, probability=TRUE, col="lightpink", main="Box-Muller: Normal Distribution")
curve(dnorm(x), add=TRUE, col="red", lwd=2) # Overlay the actual Normal Curve

