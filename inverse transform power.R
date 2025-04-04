# Inverse Transform: Power Distribution

inverse_power <- function(n){
  return(runif(n)^(1/3))
}

samples <- inverse_power(5000)
hist(samples, probability = TRUE, col='lightblue', main='Inverse Transform: Power Distribution')