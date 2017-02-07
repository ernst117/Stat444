#  The statements below make use of two R packages:  XML and plyr.
#  Both have many useful functions.  XML permits reading html tables from
#  websites.  plyr permits applying functions to parts of a large dataset.
#  

library(XML)
url <- "http://sports.yahoo.com/nba/players/3118/"
tables <- readHTMLTable(url)
pst <- tables[["Player Season Totals"]]

library(plyr)
for (i in 3:19) pst[,i] <- as.numeric(as.character(pst[,i]))

#  We will use the variables FGM and FGA:  field goals made and field
#  goals attempted.  FGM is a binomial random variable with some probability
#  theta and n = FGA.  We use a noninformative prior, and will investigate
#  whether Bryant is still improving his game.

y = pst[,5]
n = pst[,6]
cumy = cumsum(y)
cumn = cumsum(n)

post = matrix(rep(1,19000), nrow=1000, ncol=19)

#  The ith col of post contains 1000 draws from the posterior distribution that is beta
#  with parameters a = cumy[i]+1 and b = cumn[i]-cumy[i]+1.
for(i in 1:19){
  a = cumy[i]+1
  b = cumn[i]-cumy[i]+1
  for(j in 1:1000)
     post[j,i] = rbeta(1, a, b)
}


labx="Prob of making the field goal"
plot(density(post[,1]), xlim=c(0.35,0.5), ylim=c(0,155), xlab=labx, lwd=3, col="blue", main="")
lines(density(post[,5]), lwd=3, col="red")
lines(density(post[,10]), lwd=3, col="green")
lines(density(post[,19]), lwd=3, col="gray")
legend1 = c("1996-97", "1996-01", "1996-06", "1996-13")
legend("topleft", legend1, col=c("blue","red","green","gray"), lwd=c(3,3,3,3))




