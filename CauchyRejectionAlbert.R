# Code for the rejection sampling example in set of notes #4.  This is
# the example in which we draw values of mu where the sampling distribution
# is Cauchy.

# Read the pdf file entitled "Cauchy rejection example" first!

# Load LearnBayes

library(LearnBayes)
data = c(1, 2, 3, 2, 5.2, 6, 7, 8)

log.posterior = function(mu, data) {
  val = 0
  sigma = 1
  for (i in 1:length(data)) {
    val = val + log(dt((data[i] - mu)/sigma, df=1)/sigma)
  }
  return(val)
}

mu = seq(-1, 10, by=0.1)
plot(mu, exp(log.posterior(mu,data)), type="l", lwd=2)

fit = laplace(log.posterior, 2, data)
fit

var = fit$var*4
tpar=list(m = 4, var = 2.25, df=4)
datapar = list(data=data, par = tpar)

# We now find the constant c by finding the maximum of log.posterior
# minus the t proposal density.  We write a function to evaluate this
# difference.

diff = function(mu, datapar){
  val = 0*mu
  tpar = datapar$par
  mode = tpar$m
  sigma = sqrt(tpar$var)
  data = datapar$data
  df = tpar$df
  val = log.posterior(mu, data) - log(dt((mu - mode)/sigma, df=df)/sigma)
  return(val)
}

plot(mu, diff(mu,datapar), type="l", lwd=2)

# We maximize the difference over mu, to find d = exp(c), the bounding
# constant.

fit2 = laplace(diff, 2, datapar)
fit2

d = diff(fit2$mode, datapar)

# Show both the target density exp(log.posterior) and the covering 
# distribution multiplied by the constant exp(d).

plot(mu, exp(log.posterior(mu, data)), type="l", lwd=2, ylim=c(0, 4e-10))

m = tpar$m
sigma=sqrt(tpar$var)
df = tpar$df
lines(mu, dt((mu-m)/sigma, df)*exp(d)/sigma, col="blue", lwd=2)

# We can now use the rejectsampling function in LearnBayes

sample = rejectsampling(log.posterior, tpar, d, 5000, data)

f = exp(log.posterior(mu, data))
f = f/sum(f)/0.1

plot(mu, f, type="l", col="green", lwd=3, ylim=c(0, 0.37))
lines(density(sample), col="purple", lwd=3)
legend(6, 0.3, legend=c("Exact", "Simulated"), col=c("green", "purple"), lty=c(1,1), lwd=c(3, 3))

















