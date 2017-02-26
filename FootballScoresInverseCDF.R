###  Inverse CDF method to draw values of sigma^2 from its marginal
###  posterior distribution.  See class notes from 2/8.
###  The data are in LearnBayes.  We also load the MCMCpack only because the
###  function dinvgamma, which we use in the very last line of the code, is
###  in that package.  

library(LearnBayes)
install.packages("MCMCpack")
library(MCMCpack)

data(footballscores)
attach(footballscores)
y = favorite - underdog - spread

y = marathontimes$time
sd(marathontimes$time)^2
mean(y)
#######  Case 1:  conjujate prior for mu and sigma^2  #####
#  In this case we do not need to use the inverse CDF method because
#  the marginal for sigma^2 is inverted chi-square.  But we use the
#  CDF approach just for illustration.

#  We generate a grid of values of sigma^2 in the interval 150 to 
#  250 (see page 82 in Gelman et al., 2004). The vector sig2 contains
#  those values of sigma^2.

sig2 = seq(from=100, to=200, length=1000)

#  We now evaluate p(sigma^2|y) for each value of sigma^2 i the vector
#  sig2.  First we compute the values of the parameters nu_n, sigma^2_n, etc.

nu0 = 100
n = length(y)
nun = nu0 + 0.5*n

kappa0 = 100
sigma20 = 180
sigma2n = sigma20 + 0.5*(n-1)*var(y) + 0.5*kappa0*n*mean(y)*mean(y)/(kappa0+n)

#  The vector p will contain the 1000 evaluations of p(sigma^2|y), for each of
#  the 1000 values of sigma^2 in the grid which we had stored in sig2.  It is
#  easier to evaluate the log of the marginal, so we do that and save the values
#  in the vector called lp.  We then exponentiate the elements of lp and save them
#  into p.  In the final step, we normalize the elements of p by dividing each one
#  by the sum of all of them. This way, we ensure that the elements of p add up to
#  one.

p=seq(from=0, to=1, length=length(sig2))
lp = p 
for(i in 1:length(lp)){
  lp[i] = -(nun+1)*log(sig2[i]) - sigma2n/sig2[i]
}

p <- exp(lp - max(lp))
p <- p/sum(p)

#  Here we compute the empirical cdf by adding the elements of p.  The
#  elements of cdf is what we called f in the notes.

cdf <- cumsum(p)

#  Draw M = 5000 uniform random variables and for each of them, 
#  check where they fall relative to the f values.  The vector icdf contains
#  5000 draws of sigma^2 from the marginal p(sigma^2|y).

u <- runif(5000)
icdf <- rep(0,5000)
for (i in 1:5000) {
  icdf[i] <- sig2[1]
  for (j in 1:1000) {
    if (cdf[j] < u[i]) icdf[i] <- sig2[j]
  }
}

hist(icdf,freq=F)
lines(sig2,dinvgamma(sig2,nun,sigma2n))


