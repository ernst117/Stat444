########################################################################
# Exercise 2 in Lab 2
# Estimating a proportion with a discrete prior
########################################################################

# Load LearnBayes
library(LearnBayes)
p=(0:8)/8
prior=c(1,1,950,8,8,8,8,8,8)/1000
data=c(6,4)
post=pdisc(p,prior,data)
round(cbind(p, prior, post),3)
library(lattice)
pr = data.frame("Prior", p, prior)
po = data.frame("Posterior", p, post)
names(pr) = c("Type", "P", "Probability")
names(po) = c("Type", "P", "Probability")
data = rbind(pr, po)
xyplot(Probability~P|Type, lwd=3, layout=c(1,2), data=data, type="h")
sim.p=sample(p,size=1000,prob=post,replace=TRUE)
hist(sim.p, xlim=c(0,1), ylab="Frequency", xlab="Simulated p")
#Compute the posterior probs that p is in the intervals (0, .25),
# (0.25, 0.5), (0.5, 0.75), (0.75, 1)
table(cut(sim.p, breaks=seq(0, 1, by=0.25)))/1000
prop = (sum(sim.p <=0.5) - sum(sim.p <= 0.25))/1000
prop
# Predict the probability of observing 0, 2, .., 10 successes
# in n = 10 future trials. Round results to 3 digits.
round(pdiscp(p, post, 10, c(0, 2, 4, 6, 8, 10)),3)

