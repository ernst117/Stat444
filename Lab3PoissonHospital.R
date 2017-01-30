# Heart transplants. 
# Sampling model is Poisson, conjugate Gamma prior for theta.

# y is number of deaths within 30 days of surgery at a hospital.
# n is the number of heart transplant surgeries at the hospital.
# e is exposure for the hospital, which is proportional to n but
# also accounts for patient characteristics: age, other 
# co-morbidities, socio-economics, etc.  

# Prior Gamma parameters are a = 16 and b = 15174.

a = 16 ; b = 15174

# Hospital A:  1 death and exposure is 66.

yobs = 1 ; ex = 66
y = 0:10
lambda = a / b
py = dpois(y, lambda*ex)*dgamma(lambda, shape=a, rate=b)/dgamma(lambda, shape = a + y, rate=b+ex)
res.A = cbind(y, round(py, 3))
res.A
lambdaA = rgamma(1000, shape=a + yobs, rate=b+ex)
postlambdaA = quantile(lambdaA, c(0.025, 0.50, 0.975))
postlambdaA
plot(density(postlambdaA), main="Hospital A", xlab="Mortality rate", col="blue", lwd=3)

pred.poisson = function(yobs, ex)
 {
 py = dpois(y, lambda*ex)*dgamma(lambda, shape=a, rate=b)/dgamma(lambda, shape = a + yobs, rate=b+ex)
 result = cbind(y, round(py, 3))
 return(result)
 }

yobs = 4 ; ex = 1767
pred.poisson(yobs, ex)

lambdaB = rgamma(1000, shape=a + yobs, rate=b+ex)
postlambdaB = quantile(lambdaB, c(0.025, 0.50, 0.975))
postlambdaB

par(mfrow = c(2,1))
plot(density(lambdaA), main="Hospital A", xlab="Lambda for hospital A", lwd=3, ylim=c(0,2000))
curve(dgamma(x, shape=a, rate=b), add=TRUE)
legend("topright", legend=c("prior", "posterior"), lwd=c(1,3))
plot(density(lambdaB), main="Hospital B", xlab="Lambda for hospital B", lwd=3, ylim=c(0,2000))
curve(dgamma(x, shape=a, rate=b), add=TRUE)
legend("topright", legend=c("prior", "posterior"), lwd=c(1,3))

# Mixture of conjugate priors -- Exercise 7, pg 60 in Albert.  

probs = c(0.5, 0.5)
gamma.par1 = c(1.5, 1000)
gamma.par2 = c(7, 1000)
curve(0.5*dgamma(x, 1.5, 1000) + 0.5*dgamma(x, 7, 1000), from=0, to = 0.03, lwd = 3, xlab="Lambda", ylab= "Density")
gammapar = rbind(gamma.par1, gamma.par2)
y = c(4)
t = c(1767)
data = list(y=y, t=t)
post = poisson.gamma.mix(probs, gammapar, data)
post

curve(post$probs[1]*dgamma(x, 5.5, 2767) + post$probs[2]*dgamma(x, 11, 2767), from=0, to = 0.03, lwd = 3, xlab="Lambda", ylab= "Density")
curve(0.5*dgamma(x, 1.5, 1000) + 0.5*dgamma(x, 7, 1000), lwd = 1, add=TRUE)

sample1 = rgamma(760, 5.5, 2767)
sample2 = rgamma(240, 11, 2767)
post.sample = append(sample1, sample2, after = length(sample1))
summary(post.sample)
post.summary = quantile(post.sample, c(0.025,0.5, 0.975))
hist(post.sample, main="Mixture Posterior")








