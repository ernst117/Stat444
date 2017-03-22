#Problem 1:

#1.a Plot f(x) for -10 <= x <= 10.

library(LearnBayes)

x = seq(-10, 10, by=.01)
fx = (0.5*dnorm(x, 3, 2)) + (0.5*dnorm(x,-5,1))

plot(x, fx, type="l", lwd=3, col="black", main="Target Density", ylim=c(0,0.25))


##1.b Use an instrumental distribution g1(x) that is uniform in (-10, 10) and find the appropriate constant c to implement a rejection sampler. Draw n = 1000 values from f(x) using g1(x) as the proposal distribution. Compute the acceptance rate.

theta.star = runif(1000, -10, 10)
u = runif(1000, -10, 10)
pi = 3.14159

keep = 0 ; reject = 0
theta=rep(0, 1000)
prob = rep(0, 1000)
for(i in 1:length(theta.star)){
  prob[i] = ((0.5*(1/(sqrt(2*pi)*2))*exp((-1/(2*2^2))(theta.star[i]-3)^2))+(0.5*(1/(sqrt(2*pi)*1))*exp((-1/(2*1^2))(theta.star[i]-(-5))^2)))/0.2
  if(u[i] <= prob[i]){
    theta[keep] = theta.star[i]
    keep = keep + 1
  }
  else{ 
    reject = reject + 1
  }
}

prob.accept = keep / length(theta.star)
prob.accept

##1.c Now use an instrumental distribution g2(x) that is normal with mean 0 and variance 16. Find the appropriate constant c and implement a rejection sampler to draw n = 1000 values from f(x). Is this instrumental distribution more or less efficient than the one you used in part b?



#Problem 2:

#2.a

pain = read.csv(file.choose())
boxplot(Pain~Drug,data=pain, main="Pain Data", xlab="Drug", ylab="Pain")

#Answer: While the data ranges do overlap slightly, Drug A has the lowest mean headache pain and looks to significantly differ in effectiveness from drugs B and C

#2.b Write down the joint posterior distribution of model parameters (theta1; theta2; theta3; sigma2; my; tau2).

#2.c Obtain 15000 draws of the parameters in the model using JAGS (use as example the code that is posted in the Lab 7 folder).

#2.d Draw time plots, histograms of the posterior distributions of all parameters, and obtain the ACF functions to check the degree of auto-correlation between draws. What do you see? Summarize the posterior distributions by its mean, 5th and 95th percentiles.

#2.e Are there signifficant differences between drug A and drug B in their effect on migraine pain control? What about drug B and C?

#Problem 3: These data are presented by Rao (2002). Suppose that we know the genotype of 197 animals. There are four distinct genotypic groups, with the frequencies shown in Table 1.

#3.a Write the posterior distribution of n, up to the normalizing constant (i.e., you do not worry about the normalizing constant). [Hint: look at the cancer mortality example.]

#3.b Use a normal approximation to obtain a 95% credible set for n and also for the parameter in the original scale theta.

#3.c Use a rejection algorithm for simulating from the posterior density of n. Use a t-distribution as your proposal, with a small number of degrees of freedom and mean and scale parameters given by the normal approximation.

#3.d Use a Metropolis-Hastings random walk algorithm to simulate from the posterior density of n. If you use the rwmetrop in LearnBayes, use a scale factor s equal to 2 so that the standard deviation in the proposal is twice as large as the standard deviation in the normal approximation. Estimate your acceptance rate and obtain the 95% credible set for n using the simulated values.











