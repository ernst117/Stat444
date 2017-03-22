#  Half-normal example.  We use rejection sampling with an exponential 
#  proposal density.  See notes for details.  The bounding constant c
#  is equal to sqrt(2 / pi)*exp(1/2).

#  First draw p(x) and m(x).

x = seq(0, 4, by=0.01)
const = sqrt(2/3.14159)
c = const*exp(0.5)

target.p = const*exp(-x^2/2)
c.mx = c*exp(-x)

plot(x, target.p, type="l", lwd=3, col="black", main="Half Normal", ylim=c(0,1.5))
lines(x, c.mx, col="magenta", lwd=3, type="l")
leg1 = c("Target is half normal", "Proposal is exponential")
legend("topright", leg1, col=c("black", "magenta"), lty=c(1,1), lwd=c(3,3))

#  Now carry out rejection sampling.

keep = 0 ; reject = 0 ;
x = rep(0, 10000)
x.star = rexp(10000, rate=1)
head(x.star)
p.x = const*exp(-x.star^2/2)
m.x = exp(-x.star)
prob = p.x / (c * m.x)
head(p.x)
head(m.x)
head(prob)

for(i in 1:length(x.star)){
   u = runif(1, 0, 1)
   if(u <= prob[i]){
      keep = keep + 1
      x[keep] = x.star[i]
   }
   else{
      reject = reject + 1
   }
}

keep
accept.prob = keep / length(x)
accept.prob

x.plot = rep(0, keep)
for(i in 1:keep){
  x.plot[i] = x[i]
}

x = seq(0, 4, by=0.01)
const = sqrt(2/3.14159)
c = const*exp(0.5)

target.p = const*exp(-x^2/2)
c.mx = c*exp(-x)

plot(x, target.p, type="l", lwd=3, col="black", main="Half Normal", ylim=c(0,1.5))
lines(x, c.mx, col="magenta", lwd=3, type="l")
lines(density(x.plot, from=0.1, to=4), type="l", lwd=3, col="maroon")
leg1 = c("Target is half normal", "Proposal is exponential", "Approximation")
legend("topright", leg1, col=c("black", "gray", "maroon"), lwd=c(3,3,3), lty = c(1,1,1))



