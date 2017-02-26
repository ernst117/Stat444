#Problem 1: Refer to Lab 4. Re-analyze Newcomb’s speed of light experiment, but this time using a conjugate prior for μ,σ2. For σ2, select a prior scale parameter that is in agreement with the sample variance and for the prior degrees of freedom, choose a value that, relative to the sample size, does not reflect much confidence in the prior scale. For μ, choose whatever values of the prior parameters seem reasonable to you, and justify your choices.

newcomb_data = c(28, 26, 33, 24, 34, -44, 27, 16, 40, -2,
                 29, 22, 24, 21, 25, 30, 23, 29, 31, 19,
                 24, 20, 36, 32, 36, 28, 25, 21, 28, 29,
                 37, 25, 28, 26, 30, 32, 36, 26, 30, 22,
                 36, 23, 27, 27, 28, 27, 31, 27, 26, 33,
                 26, 32, 32, 24, 39, 28, 24, 25, 32, 25,
                 29, 27, 28, 29, 16, 23)
newcomb_data

#Problem 2: Albert (2011) describes a dataset that includes marathon completion times (in minutes) for men aged 20 to 29 years who participate in the New York marathon. The dataset has times for 20 runners and you can access the data from R by issuing the commands:

library(LearnBayes)
data(marathontimes)
attach(marathontimes)

#Let y1, y2, ..., y20 denote the completion times for the 20 runners and assume that yi ∼ N(μ,σ2), where both μ and σ2 are unknown. Use a conjugate prior for μ,σ2 and select values for the prior parameters which you think are good choices. Justify those choices. Carry out a test of sensitivity to the prior by setting ν0, κ0 at three different values reflecting low, medium and high confidence in your prior guesses for μ,σ2.



#Problem 3: Revisit the football scores example, where now we consider non-conjugate prior distributions for μ, σ2 (see Notes, pp 27-33). We wish to obtain the posterior distributions for the two parameters μ,σ2, and we do so under two different priors.

#3.a First choose a standard non-informative prior for μ, σ2, with p(μ, σ2) ∝ σ−2. Approxi- mate the posterior distribution of μ, σ2 by first drawing values of σ2 from its margina posterior distribution p(σ2|y) using the inverse CDF method (see Notes, p. 33) and then drawing values of μ from its conditional posterior distribution p(μ|σ2,y). Draw histograms of the posterior distributions of μ and of σ2.

#3.b Consider now a non-conjugate prior for μ, σ2 of the form 
#p(μ, σ2) = p(μ)p(σ2) = N(μ|μ0, τ02)Inv − χ2(ν0, σ02)
#for some reasonable choices of μ0,τ02,ν0,σ02. Implement the inverse CDF method to draw values from the marginal posterior distribution of σ2, and given those draws, generate values of μ from its conditional posterior distribution, as in part [3.a].