#  Trying different beta priors and different datasets 
#  to explore the shape of the prior and posterior distributions
#  of a proportion.

#  To use the BernBeta function, we have to source it.

source("BernBeta.R")

#  Beta(1,1) prior and 11 out of 14 successes
priorShape = c(1,1)
dataVec = c(1,1,1,1,1,1,1,1,1,1,1,0,0,0)
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

# Different prior
priorShape = c(100,100)
postShape = BernBeta(priorShape, dataVec, credMass)

#  Half successes
priorShape = c(1,1)
dataVec = c(1,1,1,1,1,1,1,0,0,0,0,0,0,0)
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

#  Different prior
priorShape = c(100,100)
postShape = BernBeta(priorShape, dataVec, credMass)

