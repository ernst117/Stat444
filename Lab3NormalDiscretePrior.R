library(LearnBayes)

mu=seq(20,70,by=10)
prior=c(10,15,25,25,15,10)/100
y=c(38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 
64.1, 40.1, 40.7, 6.4)
ybar=mean(y)
sigma=10; n=length(y)
like=exp(-n/2/sigma^2*(mu-ybar)^2)
post=prior*like/sum(prior*like)
dist=round(cbind(mu,post),3)
discint(dist,.8)