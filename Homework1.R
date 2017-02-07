###################################
############ Homework 1 ###########
###################################

#### Question 1 ####
##Suppose that in a population, the probability of having a rare disease is 1 in 1000. We use theta to denote the true probability of having the disease. A diagnostic test for this disease has a sensitivity of 99% and a specificity of 95%. A randomly selected person from the population is administered the test and the test and it comes up positive (the test suggests that the person has the disease). What is the probability that the person indeed is suffering from the rare disease?

disease.pos = 0.001
disease.neg = 0.999
testpos.diseasepos = 0.99
testpos.diseaseneg = 0.05
testneg.diseasepos = 0.01
testneg.diseaseneg = 0.95

diseasepos.testpos1 = (disease.pos*testpos.diseasepos)/((disease.pos*testpos.diseasepos)+(disease.neg*testpos.diseaseneg))
diseasepos.testpos1

##Answer: 0.0194

#### Question 2 ####
##Suppose that the person in Problem 1 had a second test using the same diagnostic tool (a re-test to confirm earlier results). Suppose further that the re-test is negative. What is the updated probability that the person has the disease now that the second test is available?

diseaseneg.testpos1 = 1 - diseasepos.testpos1

diseasepos.testneg1 = (diseasepos.testpos1*testneg.diseasepos)/((diseaseneg.testpos1*testneg.diseaseneg)+(diseasepos.testpos1*testneg.diseasepos))
diseasepos.testneg1

##Answer: 0.0002

#### Question 3 ####
##Consider a different person from the same population as above. The person is also tested twice for the presence of the disease. The first test is negative, but the second test is positive. What is the probability that this second person is suffering from the disease and how does this probability compare with the probability you computed in Problem 2?

diseasepos.testneg2 = (disease.pos*testneg.diseasepos)/((disease.neg*testneg.diseaseneg)+(disease.pos*testneg.diseasepos))
diseasepos.testneg2

diseaseneg.testneg2 = 1 - diseasepos.testneg2

diseasepos.testpos2 = (diseasepos.testneg2*testpos.diseasepos)/((diseaseneg.testneg2*testpos.diseaseneg)+(diseasepos.testneg2*testpos.diseasepos))
diseasepos.testpos2

##Answer: 0.002; Probabilities are the same because the samples are independent and identically distributed, so by following Baye's rule, we end on the same probabilities.

##### Question 4 #####
## An election is approaching and the latest poll of 100 people shows that 58 prefer candidate A and 42 prefer candidate B. You are curious about the chances that candidate A will prevail.

##4.1 Before you read the results of the poll, you had not thought much about the chances that candidate A would win (or a priori, you had no information about the probability of success for candidate A). What is the 95% Highest Posterior Density set (HPD set) on the probability that A will win after you have read the results of the poll?

source("BernBeta.R")
priorShape = c(1,1)
dataVec = c(rep(1, 58), rep(0,42))
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

##Answer (0.483, 0.673)

##4.2 Using simulation, compute a 95% credible set for the probability that A will win. How do the two intervals compare?

a=58; b=42

credset=qbeta(c(.025,.975),a,b)
credset

##Answer: (0.482, 0.675); They are very similar

##4.3 Just to confirm, you poll a second set of 100 randomly chosen persons from the same population. You find that in this second set, 57 prefer candidate A and the rest prefer B. How does the 95% HPD set you computed in part a change?

source("BernBeta.R")
priorShape = c(58,42)
dataVec = c(rep(1, 57), rep(0,43))
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

##Answer: (0.507, 0.643); It narrows because the second poll increases the probability that A wins

##4.4 Based on the two surveys, is it reasonable to conclude that the population is evenly divided in terms of preferences for candidates A and B?

##Answer: No, because 50% is no longer within the 95% HPD set.

##4.5 Based on the two surveys, what is the probability that candidate A will win by at least a 10% margin?

a = 115; b = 85

prob=1-pbeta(.55,a,b)
prob

##Answer: 0.764, 76.4%

#### Question 5 ####
##Suppose that two different persons (Joe and Sam) are interested in estimating the proportion theta of students at a college who commute to school. Joe uses the discrete prior shown in Table 1, while Sam decides instead to use a Beta(3; 2) prior for the proportion of commuters theta.

##5.a Use R to compute the mean and standard deviation of theta for Joe's prior and for Sam's prior. Do Joe and Sam have similar prior beliefs about the proportion of commuters?

##Joe's

prior=c(5,2,2,0.5,0.5)/10

mean(prior)
sd(prior)

##Answer: Mean = 0.2, St.Dev = 0.184

##Sam's

a=3; b=2

mean = (a/(a+b))
mean

sd = sqrt((a*b)/(((a+b)^2)*(a+b+1)))
sd

##Answer: Mean = 0.6, 0.2; No, they do not have similar prior beliefs about the proportion of commuters.

##5.b Suppose that you carry out a survey of the students in the college and fnd that out of 100 student surveyed, 30 of them commute. Use the function pdisc in LearnBayes to find the posterior probability under each of the priors that no more than 20% of the students in the college are commuters. 

##Joe's
install.packages("LearnBayes", repos = "http://cran.us.r-project.org")
library(LearnBayes)
p=(1:5)/10
data=c(30,70)
post=pdisc(p,prior,data)
round(cbind(p, prior, post),3)

##Answer:
##      p   prior post
##[1,] 0.1  0.50 0.000
##[2,] 0.2  0.20 0.055
##[3,] 0.3  0.20 0.919
##[4,] 0.4  0.05 0.026
##[5,] 0.5  0.05 0.000

##Sam's

priorShape = c(3,2)
dataVec = c(rep(1, 30), rep(0,70))
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

a = 33 ; b = 72
prob=pbeta(.2,a,b)
prob

##Answer: 0.0032

##5.c Find a 90% credible set for theta under each of the two models.

##Joe's
##By looking at the above posteriors, we see that 92% fall under 0.3.

##Sam's
credsetSam = qbeta(c(0.05, 0.95), a, b)
credsetSam
##Answer: (0.242, 0.390)

#### Question 6 ####
##In the 1998 General Social Survey, n = 129 females aged 65 and older were asked whether they were generally happy. Let Yi = 1 if the ith woman reported being generally happy and let Yi = 0 otherwise. One hundred and eighteen women (91%) reported being generally happy and 11 women (9%) said that they were not generally happy

##6.1 Consider first a uniform prior distribution for theta, the probability that a woman will report being generally happy. Simulate 1000 values from the posterior distribution of theta and summarize results.

priorShape = c(1,1)
dataVec = c(rep(1, 118), rep(0, 11))
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

posta = 119; postb = 12

sim.p=rbeta(1000,posta,postb)

summary(sim.p)
sd(sim.p)

##Answer:
## Min.   1st Qu. Median  Mean    3rd Qu.  Max. 
##0.8037  0.8928  0.9103  0.9082  0.9264  0.9686
##st dev = 0.0254

##6.2 Consider now a conjugate beta prior, such that E(theta) ~ 0.6. You are not very confident that 0.6 is a good guess for theta. Again simulate 1000 values from the posterior distribution of theta. Summarize your results.

priorShape = c(6,4)
dataVec = c(rep(1, 118), rep(0, 11))
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

posta2 = 124; postb2 = 15

sim.p2=rbeta(1000,posta2,postb2)

summary(sim.p2)
sd(sim.p2)

##Answer:
## Min.   1st Qu. Median  Mean    3rd Qu.  Max. 
##0.7788  0.8759  0.8937  0.8925  0.9117  0.9610 
## st dev = 0.0263

##6.3 Suppose that some additional information has become available and you are now quite comfortable about 0.6 as a guess for theta. Modify your prior accordingly and repeat the analysis. Comment on what you observe.

priorShape = c(600,400)
dataVec = c(rep(1, 118), rep(0, 11))
credMass = 0.95
postShape = BernBeta(priorShape, dataVec, credMass)

posta3 = 718; postb3 = 411

sim.p3=rbeta(1000,posta3,postb3)

summary(sim.p3)
sd(sim.p3)

##Answer:
## Min.   1st Qu. Median  Mean    3rd Qu.  Max. 
##0.5875  0.6273  0.6372  0.6369  0.6470  0.6798 
##st dev = 0.0141

##6.4 Under the model in 6.3, compute the posterior predictive distribution of observing between 80 and 90 women who report being happy if an additional 100 women were to be interviewed.

ab=c(718,411)
n=100
s=80:90
pred = pbetap(ab,n,s)

sum(pred)

##Answer: 0.0004
