model{
  for(i in 1:n){
    y[i] ~ dnorm(mu, tau2)
  }
  mu ~ dnorm(5.0, 1/tau2)
  tau2 ~ dgamma(0.001, 0.001)
  sigma <- sqrt(1/tau2)
}