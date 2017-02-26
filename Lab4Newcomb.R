#  Newcomb's speed of light experiment.  Data are stored in the 
#  file called newcomb.txt, saved in the same directory.
#  The prior distribution for mu and sigma^2 is non-informative, 
#  and proportional to a uniform (for mu) times an inverted chi^2
#  with zero degrees of freedom (for sigma^2).

y = scan("newcomb.txt")
y = newcomb_data

#  Compute the sum of squares and n and then draw 1000 values of
#  sigma^2 from the scaled - inverted chi^2 distribution with n-1
#  degrees of freedom. Recall that we have to first draw the chi^2
#  random variable and then invert it to get sigma^2, because there
#  is no routine in R that will draw values directly from an inv-X^2.

S=sum((y-mean(y))^2)
n=length(y)
sigma2=S/rchisq(1000, n - 1)
par(mfrow=c(2,1))
hist(sigma2)

#  Now we draw values of mu from its conditional posterior, given
#  sigma^2.  This conditional posterior has mean ybar = mean(y)
#  and variance sigma^2/n.

mu = rnorm(1000, mean = mean(y), sd = sqrt(sigma2)/sqrt(n))
hist(mu)

#  95% credible sets for mu and for sigma^2

int.est.mu=quantile(mu,c(.05, .95))
int.est.mu
int.est.sigma=quantile(sqrt(sigma2),c(0.05, .95))
int.est.sigma

#  Posterior distribution for the 75th posterior quantile of mu.
#  Notice how easy it is to obtain the posterior distribution of a
#  function of parameters.

p.75=mu+0.674*sqrt(sigma2)
hist(p.75)
post.mean=mean(p.75)
post.sd=sd(p.75)

#  Posterior distribution for the coefficient of variation, defined
#  as sigma / mu is a measure of dispersion in the population.

post.cv = sqrt(sigma2) / mu
summary(post.cv)
hist(post.cv)

#  Posterior predictive checking.  We generate 20 replicate samples of size 
#  66.  In each sample we compute miny, the minimum value of y.  We compare
#  the observed value -44 with the distribution of minima that we get from
#  the M = 20 replicate datasets.

M = 100
s2 = numeric(M) ; m = numeric(M)
s2 = S/rchisq(M, n - 1)
m = rnorm(M, mean = mean(y), sd = sqrt(s2)/sqrt(n))
yrep = matrix(0, nrow = M, ncol=n)
for(i in 1:M){
   for(j in 1:n){
      yrep[i,j] = rnorm(1, mean=m[i], sd=sqrt(s2[i]))
   }
}

ymin = numeric(M)
for(i in 1:M) ymin[i] = min(yrep[i,])
ymin

hist(ymin, xlim=c(-50, 20))
abline(v = -44, col="red", lwd = 3)

     
 

