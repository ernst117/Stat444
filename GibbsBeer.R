#  Simple normal linear model to illustrate the Gibbs sampler.  We 
#  use alcohol content of beers as the response variable and let
#  y ~ N(mu, sigma^2), with both mu and sigma^2 unknown.  We use a
#  conjugate prior for mu and sigma^2.  The two conditional distributions
#  are:  normal (for mu) and inverted gamma (for sigma^2).

beer = read.table("beer.txt")
names(beer) = c("brand","price","calories","alcohol","type","domestic")
  # type: 1=craft lager, 2=craft ale, 3=import lager,
  #    4=regular & ice beer, 5=light/no alcohol beer
  # domestic: 1=U.S., 2=import
beer = beer[1:62,]  # remove non-alcoholic beers

dim(beer)

attach(beer)
hist(alcohol)

n = length(alcohol)
ybar = mean(alcohol)

#priors: mu|sigma2 ~ N( 5, sigma2),  sigma2 ~ Inverse-Gamma(1, 1), thus:
m = 5
s0 = 1
alpha = 1
beta = 1

N = 1200
mu = numeric(N)
sigma2 = numeric(N)
mu[1] = 5
sigma2[1] = 1
for(i in 1:(N-1)) {
  mu[i+1] = rnorm(1, mean=( (n*ybar + s0*m)/(n + s0) ),
       sd=sqrt( sigma2[i]/(n + s0) ) )
  ysum = 0
  for(j in 1:n) ysum = ysum + (alcohol[j] - mu[i+1])^2
  sigma2[i+1] = 1/rgamma(1, (n+1)/2+alpha, 0.5*ysum + 0.5*s0*(mu[i+1]-m)^2 + beta)
}

par(mfrow=c(2,1))
t = seq(from = 1, to = 1200)
plot(t, mu, type ="l", main="Time plot for mu", xlab="Iteration")
abline(h = mean(mu), col="red")
plot(t, sigma2, type ="l", main="Time plot for sigma^2", xlab="Iteration")
abline(h = mean(sigma2), col="red")

mu = mu[201:1200]
sigma2 = sigma2[201:1200]

t = seq(from = 201, to = 1200)

plot(t, mu, type="l", main="Draws of mu")
plot(t, sqrt(sigma2), type="l", main="Draws of sigma")

acf(mu)
acf(sigma2)

par(mfrow=c(1,2))
hist(mu)
hist(sigma2)

mean(mu)
sd(mu)

mean(sigma2)

#  NOW DO THE SAME ANALYSIS BUT USING JAGS, just another gibbs sampler
#  We need to load the package rjags and write a file with the model.

install.packages("rjags")
install.packages("coda")
library(rjags)
library(coda)

y = alcohol
model = jags.model("BeerModel.txt", data=list("y"=y, "n"=n),
                   n.chains = 3)

#  Draw samples from the model.  We get 1000 iterations from each of the 3 chains.
chains = coda.samples(model=model, c("mu", "tau2", "sigma"), n.iter=1000)

#  We can use the mcmc object chains directly, to draw plots or do diagnostics.
plot(chains)
par(mfrow=c(3,1))
autocorr.plot(chains, 20, auto.layout = FALSE)
gelman.diag(chains, confidence=0.95, transform=FALSE, autoburnin=TRUE)
gelman.plot(chains, bin.width=10, max.bins=50, confidence=0.95, transform=FALSE,
    autolayout = FALSE)

# Extract the three chains from the mcmc.list object.
chain1 = chains[[1]]
chain2 = chains[[2]]
chain3 = chains[[3]]

draws = rbind(chain1, chain2, chain3)

par(mfrow=c(2,1))
plot(density(draws[,1]), lwd=2, main="Posterior of mu", xlab="mu")
plot(density(draws[,2]), lwd=2, main="Posterior of sigma", xlab="sigma")



