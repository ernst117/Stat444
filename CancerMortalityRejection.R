## Example: Cancer mortality example using rejection sampling.
## In LearnBayes, the function rejectsampling uses a t distribution
## as the proposal.

#  Load LearnBayes.

library(LearnBayes)
data(cancermortality)
attach(cancermortality)
head(cancermortality)

#  First, maximize the target distribution to find its mode and
#  and an approximation to its variance.  We use (-7, 6) as the
#  initial guesses for logit.eta and log.k.

logit.eta = -7
log.k = 6
fit = laplace(betabinexch, c(-7,6), cancermortality)
fit

#  We first find the bounding constant d = log c by maximizing the
#  difference between the log target and the log proposal 
#  distributions.  We use a t distribution with low df as
#  the proposal.  We use the two-dimensional vector theta to save
#  logit.eta and log.k.  The function betabinT computes the difference
#  between log posterior and log proposal, which is a function of d.

theta=numeric(2)
tpar=list(m=fit$mode, var=2*fit$var, df=4)
datapar = list(data=cancermortality, par = tpar)
data=cancermortality

betabinT = function(theta, datapar) {
  data = datapar$data
  tpar = datapar$par
  d = betabinexch(theta,data) - dmt(theta,mean=c(tpar$m),S=tpar$var,df=tpar$df,log=TRUE)
  return(d)
}

#  To find the maximum of d, call the function betabinT and use laplace.

start=c(-6.9, 12.4)
fit1 = laplace(betabinT, start, datapar)
fit1  
betabinT(fit1$mode, datapar)

#  Carry out rejection sampling.  Inputs are the log posterior, the
#  parameters of the t proposal distribution, the maximum value of
#  d, the number of draws and the data.

theta=rejectsampling(betabinexch,tpar,-569.2813,5000,cancermortality)

#  Investigate efficiency of algorithm and estimate how many draws I
#  would need to end up with a sample of size 2000.
 
dim(theta)
n.accept = length(theta[,1])
n.accept
prob.accept = n.accept / 5000
draws = 2000 / prob.accept
draws

#  Plot simulated draws on the contours of the log-posterior density.

mycontour(betabinexch, c(-8, -4.5, 3, 16.5), cancermortality, xlab="logit eta", ylab="log K")
points(theta[,1], theta[,2], col="turquoise3")

#  We can also plot the marginal posterior distributions of the parameters
#  in the original scale.

eta = exp(theta[,1]) / (1 + exp(theta[,1]))
K = log(theta[,2])

par(mfrow=c(2,1))
plot(density(eta), xlab="eta", main="Marginal posterior distribution of eta")
plot(density(K), xlab="K", main="Marginal posterior distribution of K")













