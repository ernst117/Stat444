#  Simple rejection sampling example.  Target distribution p(theta) is Beta(2,2)
#  and proposal m(theta) is uniform(0,1).  We find the bounding constant by
#  differentiating the ratio p / m with respect to theta.  For this example,
#  c = 3/2.

#  First draw 10000 theta.star from m(theta) = U(0,1) and 10000 uniforms from
#  U(0,1).

theta.star = runif(10000, 0, 1)
u = runif(10000, 0, 1)

#  Now loop over the 10000 candidate values theta.star to see which ones
#  we keep and which we reject.  We initialize two indices, keep and reject.
#  The vector prob contains the acceptance probabilities of each theta.star.

keep = 0 ; reject = 0
theta=rep(0, 10000)
prob = rep(0, 10000)
for(i in 1:length(theta.star)){
  prob[i] = 6 * theta.star[i] * (1 - theta.star[i]) / 1.5
  if(u[i] <= prob[i]){
    theta[keep] = theta.star[i]
    keep = keep + 1
  }
  else{ 
    reject = reject + 1
  }
}

prob.reject = reject / length(theta.star)
prob.reject

#  Plot the target distribution, c*m and the resulting approximation
#  on the same figure.

theta.plot = rep(0, keep)
for(i in 1:keep){
  theta.plot[i] = theta[i]
}

x = rbeta(500000, 2, 2)
plot(density(x), type="l", lwd=3, xlab="theta", ylim=c(0,2), main="Beta(2,2) approx.")
abline(h=1.5, lwd=3, col="red")
lines(density(theta.plot), lwd=3, col = "gray")
 

