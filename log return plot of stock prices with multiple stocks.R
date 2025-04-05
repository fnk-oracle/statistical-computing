# Extending the last stock price simulation we will add few things;
# Extending the simulation to multiple stocks
# Add confidence intervals or shaded regions
# Plot log returns instead of prices

box_muller <- function(n){
  u1 <- runif(n/2)
  u2 <- runif(n/2)
  r <- sqrt(-2 * log(u1))
  z1 <- r * cos(2 *pi * u2)
  z2 <- r * sin(2 * pi * u2)
  return(c(z1,z2))
}

simulate_stock <- function(s0, mu, sigma, days, n_stocks){
  prices_matrix <- matrix(0, nrow=days, ncol=n_stocks)
  
  for(i in 1:n_stocks){
    z <- box_muller(days)
    returns <- mu +sigma*z
    prices <- s0 * cumprod(exp(returns))
    prices_matrix[,i] <- prices
  }
  return(prices_matrix)
}

#adding confidence intervals(shaded areas)
plot_stock_with_ci <- function(price_matrix){
  avg_price <- rowMeans(prices_matrix)
  std_error <- apply(prices_matrix, 1, sd) / sqrt(ncol(prices_matrix))
  
  upper <- avg_price + 1.96 *std_error
  lower <- avg_price - 1.96 *std_error
  
  days <- 1:nrow(prices_matrix)
  
  plot(days, avg_price, type="l", lwd=2, col="blue",
       ylim = range(lower,upper),
       main="Simulated Stock Prices with 95% CI",
       xlab="Days",
       ylab="Average Price"
      )
  polygon(c(days, rev(days)), c(upper,rev(lower)), col=rgb(0, 0, 1, 0.2), border=NA)
  lines(days, upper, col="blue", lty=2)
  lines(days, lower, col="blue", lty=2)
}

#Plot log returns instead of prices
plot_log_returns <- function(prices_matrix){
  log_returns <- diff(log(prices_matrix[,1]))
  plot(log_returns, type="l", col="darkgreen", lwd=2,
       main="Log returns of first simulated stock",
       xlab="days",
       ylab="Log Returns")
}
#Parameters
s0 <- 100
mu <- 0.0005
sigma <- 0.02
days <- 252
n_stocks <- 50

prices_matrix <- simulate_stock(s0, mu, sigma, days, n_stocks)
plot_stock_with_ci(prices_matrix)
plot_log_returns(prices_matrix)

