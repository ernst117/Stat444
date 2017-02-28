#  SIR for cancer mortality example.  We first write our own algorithm
#  to implement SIR and then use the one-line built-in function called
#  sir in the LearnBayes package.

#  Load LearnBayes.

library(LearnBayes)
data(cancermortality)
attach(cancermortality)
head(cancermortality)

n = cancermortality[,2]
y = cancermortality[,1]


samplesize = 20
logit.eta = -7 #choices from the contour plot
log.k = 6
fit = laplace(betabinexch, c(-7,6), cancermortality) #finding mode to define parameters
fit

tpar = list(m = fit$mode, var=2*fit$var, df=4) #
datapar = list(data=cancermortality, par=tpar)

#  Draw theta from the multivariate t with 4 df.

M = 10000
theta=c(0, 0)
theta = rmt(M, mean=c(tpar$m), S=tpar$var, df = tpar$df)

log.like=numeric(M)
for(i in 1:M){
   log.like[i] = betabinexch(theta[i,], cancermortality) }

log.prop = numeric(M)
log.prop = dmt(theta, mean=c(tpar$m), S=tpar$var, df = tpar$df, log=TRUE)

#  Now we can compute the SIR weights and probabilities.

md = max(log.like - log.prop)
w = exp(log.like - log.prop - md)
hist(w, breaks = 50)

#  Check whether the weights are homogeneous enough.  Compute the effective
#  sample size ESS.  Check whether ESS < 5.

quant = ((w/mean(w)) - 1)^2
ESS = sqrt(sum(quant)/length(w))
ESS

#  Another way to look at ESS is to compute (sum w)^2 over the the sum of the
#  squared w.  We compare this value to length(w).

ESS = (sum(w))^2 / sum(w^2)
ESS

probs = w / sum(w)

#  We sample from the set of the M thetas.  We store the "label" of 
#  the sampled thetas into a vector called indices.  We then just 
#  pick the thetas from the M available by just using those indices.
  
indices = sample(1:M, size = 1000, prob=probs, replace=TRUE)
theta.sir = theta[indices,]

mycontour(betabinexch, c(-8, -4.5, 3, 16.5), cancermortality, xlab="logit eta", ylab="log K")
points(theta[,1], theta[,2], col="gray80")
points(theta.sir[,1], theta.sir[,2], col="magenta")


#  The same algorithm is implemented by the function sir in the
#  LearnBayes package.  We can get a similar set of thetas simply by
#  calling the function.

theta.sir2 = sir(betabinexch, tpar, 1000, cancermortality) #name of function that will evaluate the data
#name of function to evaluate the draws
#list for t-dist
#how many draw
#name of dataset

#  We now use SIR to check for influential observations.  
#  We ask ourselves how the 90% credible set for log K will change 
#  if we leave out one observation at a time.

summary = quantile(theta.sir2[,2], c(0.05, 0.5, 0.95))
summary.eta = quantile(theta.sir2[,1], c(0.05, 0.5, 0.95))

summary
summary.eta

summary.obs = matrix(nrow=samplesize, ncol=3, byrow=TRUE)
summary.obs.eta = matrix(nrow=samplesize, ncol=3, byrow=TRUE) #undo log transform

for(i in 1:samplesize){ ##removing one obs at a time and recomputing the 90% cred set for K and eta, evaluating the weights of each data point
  sumw = 0
  for(j in 1:1000){
    eta = exp(theta.sir2[,1])/(1+exp(theta.sir2[,1]))
    K = exp(theta.sir2[,2])
    weight = exp(lbeta(K*eta,K*(1-eta))-lbeta(K*eta+y[i],K*(1-eta)+n[i]-y[i]))
    sumw = sumw + weight
  }
  probs = weight/sumw #normalizing the weights
  indices = sample(1:1000,size=1000,prob=probs,replace=TRUE)
  theta.s=theta.sir2[indices,]
  summary.obs[i,]=quantile(theta.s[,2],c(0.05, 0.5, 0.95))
  summary.obs.eta[i,]=quantile(theta.s[,1],c(0.05, 0.5, 0.95))
}

plot(c(0,0,0),summary,type="b",lwd=3,col="maroon",xlim=c(-1,21),ylim=c(5,11),xlab="Observation removed", ylab="log K",
   main="Effect on log(K)")
for(i in 1:20) lines(c(i,i,i), summary.obs[i,], type="b")

plot(c(0,0,0),summary.eta,type="b",lwd=3,col="turquoise2",xlim=c(-1,21),ylim=c(-7.5, -6),xlab="Observation removed", ylab="logit eta",
   main="Effect on logit(eta)")
for(i in 1:20) lines(c(i,i,i), summary.obs.eta[i,], type="b") # looking at the influence of each data point



