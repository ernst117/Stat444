#  We analyze the coal mining disaster data from the UK.  The data were described
#  by Carlin et al. (1992).  D is the vector with the number of disasters per year
#  starting in 1851.   

library(rjags)

N = 112
D = c(4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2,
+   6, 3, 3, 5, 4, 5, 3, 1, 4, 4, 1, 5, 5, 3, 4, 2, 5, 2,
+   2, 3, 4, 2, 1, 3, 2, 1, 1, 1, 1, 1, 3, 0, 0, 1, 0, 1, 1, 0, 0,
+   3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 2, 1, 0, 
+   0, 0, 1, 1, 0, 2, 2, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4, 2, 0, 0, 0, 
+   1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0)

length(D)
y = seq(from=1851, to=1962)
length(y)
#  We plot the observations by year.  The number of disasters appears to decrease
#  around the end of the 19th century.

plot(y, D, type="b", xlab="Year", ylab="Number of disasters")

#  Call JAGS to fit the model.  The model says that D_i is Poisson with
#  mean mu.  Log(mu) = b_0 + I * (year - changeyear) * b_1, where the indicator
#  function I takes on the value 0 when year <= changeyear and takes on the
#  value 1 when year > changeyear.  That means that the value of log(mu) before
#  changeyear is b_0 and after changeyear is b_0 + b_1.

model = jags.model("ChangePointModel.txt", data=list("N" = N, "D" = D),
                   n.chains = 3)

#  Draw samples from the model.  We get 1000 iterations from each of the 3 chains.
chains = coda.samples(model=model, c("b[1]", "b[2]", "changeyear"), n.iter=1000)

summary(chains)
xyplot(chains)
acfplot(chains)
densityplot(chains)

We can compute the DIC for this model using the dic.samples function in rjags.
dic = dic.samples(model, n.iter = 1000, thin=1)
dic

