# Simulating Stock Market Returns using Box-Muller 
# Stock returns are often modeled as a Normal Distribution
# If daily returns follow N(mu, sigma square), we can simulate stock price movements

box_muller <- function(n){
  u1 <- runif(n/2) # First random number (from 0 to 1)
  u2 <- runif(n/2) # Second random number (from 0 to 1)
  
  z1 <- sqrt(-2 * log(u1)) * cos(2* pi *u2) # first transformed number
  z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2) #second transformed number
  
  return(c(z1,z2)) # combine both sets
}

simulate_stock <- function(s0, mu, sigma, days){
  returns <- mu + sigma * box_muller(days)
  prices <- s0 *cumprod(exp(returns))
  return(prices)
}


#Parameters
s0 <- 100 #initial stock price
mu <- 0.0005 #Average daily return (0.05% per day)
sigma <- 0.02 #daily volitality (2%)
days <- 252 # trading days in a year

prices <- simulate_stock(s0, mu, sigma, days)

plot(prices, type="l", col="blue", lwd=2, main="Simulated Stock Prices", xlab="Days", ylab="Price")

