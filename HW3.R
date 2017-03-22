#Problem 1: We wish to use rejection sampling to draw values from a mixture of normal distributions. Let x ~ f(x) = 1/2 N(3; 2) + 1/2 N(􀀀5; 1); for x 2 <. [Hint: Find c analytically or use the difference approach and find d.]

#1.a Plot f(x) for -10 <= x <= 10.

#



#1.b Use an instrumental distribution g1(x) that is uniform in (-10, 10) and find the appropriate constant c to implement a rejection sampler. Draw n = 1000 values from f(x) using g1(x) as the proposal distribution. Compute the acceptance rate.

#1.c Now use an instrumental distribution g2(x) that is normal with mean 0 and variance 16. Find the appropriate constant c and implement a rejection sampler to draw n = 1000 values from f(x). Is this instrumental distribution more or less efficient than the one you used in part b?

#Problem 2: A drug company wants to know which of three possible drug formulations is most effective against migraine headaches. Ten volunteers were randomly allocated to each of the three drugs(A, B or C) and were asked to report the level of pain during their next migraine episode, an hour after taking the drug. Participantas were instructed to use a scale from 0 (no pain) to 10 (worst pain ever) to rate their pain. Data are stored in the le entitled Pain.csv in the Homework 3 folder. Assume that a simple linear model:

#yij = mu + betai + eij,

# is appropriate, where yij is the response of the jth participant allocated to the ith drug, mu is an overall mean, betai is the effect of the ith drug, and eij is a random residual. Here, j = 1,...,10, and i = 1, 2, 3. We let thetai = mu + betai and assume that

#yij ~ iidN(thetai, sigma2)

#to complete the model, we let:

#thetai ~ N(mu, tau2)
#sigma2 ~ Inv-Chi2(nu0, sigma0)
#mu, tau2 is directly proportional to tau^-2

#and choose values nu0 = 5, sigma0 = 9

#2.a Explore the data by drawing boxplots of pain scores for each drug. From the plots, do you expect to find that drugs differ in terms of effectiveness?

#2.b Write down the joint posterior distribution of model parameters (theta1; theta2; theta3; sigma2; my; tau2).

#2.c Obtain 15000 draws of the parameters in the model using JAGS (use as example the code that is posted in the Lab 7 folder).

#2.d Draw time plots, histograms of the posterior distributions of all parameters, and obtain the ACF functions to check the degree of auto-correlation between draws. What do you see? Summarize the posterior distributions by its mean, 5th and 95th percentiles.

#2.e Are there signifficant differences between drug A and drug B in their effect on migraine pain control? What about drug B and C?

#Problem 3: These data are presented by Rao (2002). Suppose that we know the genotype of 197 animals. There are four distinct genotypic groups, with the frequencies shown in Table 1.

#3.a Write the posterior distribution of n, up to the normalizing constant (i.e., you do not worry about the normalizing constant). [Hint: look at the cancer mortality example.]

#3.b Use a normal approximation to obtain a 95% credible set for n and also for the parameter in the original scale theta.

#3.c Use a rejection algorithm for simulating from the posterior density of n. Use a t-distribution as your proposal, with a small number of degrees of freedom and mean and scale parameters given by the normal approximation.

#3.d Use a Metropolis-Hastings random walk algorithm to simulate from the posterior density of n. If you use the rwmetrop in LearnBayes, use a scale factor s equal to 2 so that the standard deviation in the proposal is twice as large as the standard deviation in the normal approximation. Estimate your acceptance rate and obtain the 95% credible set for n using the simulated values.











