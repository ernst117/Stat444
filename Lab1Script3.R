#  Draw values from an exponential distribution with parameter theta = 1.


par(mfrow=c(1,1))
m = 1000
u = runif(m, 0, 1)
for(i in 1:m){ x = -log(1-u) }

hist(x)


#  Draw values from a triangular distribution.

m = 1000
u = runif(m, 0, 1)
invcdf.func <- function(u) {
  if (u >= 0 && u < 0.25)
    sqrt(u)/2
  else if (u >= 0.25 && u <= 1)
    1 - sqrt(3 * (1 - u))/2
}
x <- unlist(lapply(u, invcdf.func))

fx=x

#  To compare, we compute the real density
for (i in 1:m) {if(x[i] < 0.25) fx[i] = 8*x[i]}
for (i in 1:m) {if(x[i] >= 0.25) fx[i] = (1 - x[i])*8/3}

plot(x,fx,type="p", col="gray")
lines(density(x), lty=1, col="blue", lwd=3)