# First install and then load package LearnBayes.

library(LearnBayes)
# Albert, Ch. 1 - Exploring studentdata
data(studentdata)
attach(studentdata)
studentdata[1:10,]
table(Number)
summary(Number)
barplot(table(Number), xlab="Number", ylab="Count")
hours.of.sleep = WakeUp - ToSleep
mean(hours.of.sleep)
mean(hours.of.sleep, na.rm=TRUE)
sd(hours.of.sleep, na.rm=TRUE)
hist(hours.of.sleep)
boxplot(hours.of.sleep~Gender, ylab="Hours of Sleep")
female.Number=Number[Gender=="female"]
male.Number=Number[Gender=="male"]
summary(Number)
summary(female.Number)
summary(male.Number)
plot(ToSleep, hours.of.sleep, xlab="Bedtime", ylab="Hours of Sleep")
plot(jitter(ToSleep), jitter(hours.of.sleep), xlab="Bedtime", ylab="Hours of Sleep")

# Simple linear regression

fit=lm(formula=hours.of.sleep ~ ToSleep)
fit
abline(fit)

# Writing a function that we can call repeatedly.
# The function tstatistic depends on two quantities x and y.
# To call the function, first set values for x and y and then call by
# issuing the command tstatistic(x,y).

tstatistic = function(x,y)
{
m = length(x); n = length(y)
sp = sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
t.stat = (mean(x) - mean(y))/(sp*sqrt(1/m+1/n))
return(t.stat)
}

x=rnorm(20, mean=30, sd=3)
y=rnorm(30, mean=45, sd=3)
tstatistic(x,y)

# A Monte Carlo study to rest the robustness of the t statistic when
# assumptions hold (first case) or a violated (second case).

alpha = 0.1; m=10; n=10
N = 10000
n.reject = 0
for(i in 1:N)
{
x=rnorm(m, mean=0, sd=1)
y=rnorm(n, mean=0, sd=1)
t.stat=tstatistic(x,y)
if(abs(t.stat)>qt(1-alpha/2, m+n-2)) n.reject=n.reject+1
}
true.sig.level=n.reject/N
true.sig.level

n.reject=0
for(i in 1:N)
{
x=rnorm(m, mean=0, sd=1)
y=rnorm(n, mean=0, sd=10)
t.stat=tstatistic(x,y)
if(abs(t.stat)>qt(1-alpha/2, m+n-2)) n.reject=n.reject+1
}
true.sig.level=n.reject/N
true.sig.level

# The function my.tsimulation draws the sampling distribution for the 
# t statistic when x is normal and y is exponential.  Sample sizes are
# small.

m = 10 ; n = 10
my.tsimulation=function() tstatistic(rnorm(m,mean=10,sd=2), rexp(n,rate=1/10))
tstat.vector=replicate(10000, my.tsimulation())
plot(density(tstat.vector), xlim=c(-5,8), ylim=c(0,0.4), lwd=3)
curve(dt(x, df=18), add=TRUE)
legend(4, 0.3, c("exact", "t(18)"), lwd=c(3,1))











