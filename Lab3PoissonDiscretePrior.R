## Estimating the Poisson rate with a discrete prior ##


# define the values of lambda and the associated probabilities

lambda=seq(.5,3,by=.5)
prior=c(.1,.2,.3,.2,.15,.05)

# define the data (t and y)

t=6; y=12

# compute the posterior probabilities

like=exp(-t*lambda)*(t*lambda)^y
post=prior*like/sum(prior*like)

output = round(cbind(lambda, prior, post), 3)

# compute the predictive probability that there are no breakdowns

pred.lam=exp(-7*lambda)
pred.0.breakdowns=sum(post*pred.lam)
pro