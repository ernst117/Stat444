#########################################################################
# Exercise 2
# Estimating a proportion and prediction of a future sample
########################################################################

# Define the parameters of the beta posterior

a=23; b=8

# Use the qbeta function to construct a 90% interval estimate

interval.est=qbeta(c(.05,.95),a,b)
interval.est
# Find P(p > .6) by use of the pbeta function

prob=1-pbeta(.6,a,b)
prob

# Simulate 1000 draws from the beta(a, b) distribution

sim.p=rbeta(1000,a,b)

# Simulate 1000 draws from the posterior predictive distribution

sim.y=rbinom(1000,10,sim.p)
hist(sim.y, xlim=c(0,10), xlab="Predicted # of grads")

# Tabulate the predictive distribution

table(sim.y)

# Compute the predictive probability that 9 or 10 will graduate
# by inspection, you compute this probability from the table.  Or do

prob1 = sum(sim.y >= 9)/1000
prob1
# Compute the probability of observing between 4 and 8 graduates.
prob2 = (sum(sim.y <= 8) - sum(sim.y <= 4))/1000
prob2
# Summarize the posterior predictive distribution of the number of
# among 10 future students.
summary(sim.y)
