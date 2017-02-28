#  Bioassay example - pages 69 to 74 in Albert and 88 to 93 in Gelman et al.

library(LearnBayes)
x = c(-0.86, -0.3, -0.05, 0.73)
n = c(5, 5, 5, 5)
y = c(0, 1, 3, 5)
data = cbind(x, n, y)

#  First, estimate the parameters alpha and beta using ML

response = cbind(y, n-y)
results = glm(response ~ x, family=binomial)
summary(results)

#  Now become a Bayesian.  First we select the beta priors that match
#  our information about probability of death at a low and a high dose.
#  We use -0.7 and 0.6 as the low and the high doses, respectively.
#  At the low dose, we guess that the median probability of death
#  is 0.2 and the 90th percentile of the probability of death is 0.5.
#  At the high dose, the prior median and 90th percentile are 0.9 and
#  0.98, respectively.

low.dose = beta.select(list(p=0.5, x=0.2), list(p=0.9, x=0.5))
high.dose = beta.select(list(p=0.5, x=0.8), list(p=0.9, x=0.98))
low.dose
high.dose

#  Now enter the new "data"

prior = rbind(c(-0.7, 4.68, 1.12), c(0.6, 2.84, 2.10))
data.new = rbind(data, prior)
data.new

#  Use the LearnBayes function logisticpost to obtain the posterior
#  distribution of alpha and beta. Draw the contours of the joint
#  posterior and overlay 1000 draws from the posterior.  The draws can
#  be obtained by using the function simcontour.

mycontour(logisticpost, c(-3, 3, -1, 11), data.new, xlab="alpha", ylab="beta")
s = simcontour(logisticpost, c(-3, 3, -1, 11), data.new, 1000)
points(s)

summary(s$x)
quantile(s$x, c(0.025, 0.975))
summary(s$y)
quantile(s$y, c(0.025, 0.975))
plot(density(s$y), xlab="beta", main="Marginal posterior of beta")
plot(density(s$x), xlab="alpha", main="Marginal posterior of alpha")

#  Posterior distribution of the LD50

ld50 = -s$x / s$y
quantile(ld50, c(0.025, 0.975))
plot(density(ld50), xlab="LD50", main="Posterior of LD50")




