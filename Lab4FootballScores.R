# Point spread example.  Data are in file called
# footballscores in LearnBayes.
# We assume that the difference between observed game outcome and
# point spread is N(0, sigma^2).  We consider two different priors
# for sigma^2.

library(LearnBayes)
data(footballscores)
attach(footballscores)

sd(d)
#  How many observations?  How many variables?
dim(footballscores)
footballscores[1:10,]

#  Compute outcome and difference between outcome and spread.
outcome = favorite - underdog
d = outcome - spread
n = length(d)
v = sum(d^2)
ssqr = v / n
ssqr
# Use first a non-informative distribution for sigma^2.
precision = rchisq(1000, n) / v

sigma = 1 / sqrt(precision)
hist(sigma, main="")

# Now use an inverted chi^2 prior with sigma^2_0 = 10 and
# nu_0 = 300.

nu.zero = 300
sigmasq.zero = 10
nu.n = nu.zero + n
sigmasq.n = (n*v + nu.zero*sigmasq.zero) / nu.n
prec.inf = rchisq(1000, nu.n) / sigmasq.n
sigma.inf = 1 / sqrt(prec.inf)
hist(sigma.inf, main="")
summary(sigma)
summary(sigma.inf)
quantile(sigma, probs=c(0.025, 0.5, 0.975))
quantile(sigma.inf, probs=c(0.025, 0.5, 0.975))

#  Write a function to compute the posterior median and
#  the 95% credible set for sigma, for different values of
#  of the prior scale and the prior degrees of freedom. 
#  The function can be used to do sensitivity analysis.

sigma.post = function(nu.zero, sigmasq.zero)
  {
    nu.n = nu.zero + n
    sigmasq.n = (n*v + nu.zero*sigmasq.zero) / nu.n
    prec.inf = rchisq(1000, nu.n) / sigmasq.n
    sigma.inf = 1 / sqrt(prec.inf)
    quantile(sigma.inf, probs=c(0.025, 0.5, 0.975))
    post = quantile(sigma.inf, probs=c(0.025, 0.5, 0.975))
    return(post)
  }

nu.zero = 26; sigmasq.zero = 115
sigma.post(nu.zero, sigmasq.zero)


  

