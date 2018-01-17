rm(list=ls())

#a)
#MS
MS<-read.csv("MS.csv")
x_array <- rep(0,nrow(MS)-1)

for (i in 1:nrow(MS)-1){
  x_array[i] = -100*log(MS$Adj.Close[i+1]/MS$Adj.Close[i])
}


blocksize <- 26
blocks <- floor(length(x_array)/blocksize)

max_array <- rep(0,blocks)

for (i in 1:blocks){
  start_ind <- (i - 1)*26
  end_ind  <- start_ind + 25
  max_array[i] = max(x_array[start_ind:end_ind])
}

hist(max_array, 
     main="Histogram of MSMax", 
     xlab="MSMax",
     breaks = 15)




#b)
library(qrmdata) # for the S&P 500 data
library(xts) # for functions around time series objects
library(QRM) # for fit.GEV() and fit.GPD()
library(qrmtools) # for returns() <- be RYre to load "qrmtools" last
fit.week <- fit.GEV(max_array)
(xi.week <- fit.week$par.ests[["xi"]])
(mu.week <- fit.week$par.ests[["mu"]])
(sig.week <- fit.week$par.ests[["sigma"]])
fit.week

#c)
q.week <- qGEV(ppoints(length(max_array)), xi.week, mu.week, sig.week) # contruct theoretical quantiles
qqplot(q.week, as.vector(max_array), xlab = "Theoretical Quantile (Fitted GEV)",
       ylab = "Empirical Quantile") # compare weekly maxima with theoretical quantiles
abline(0,1) # 45 degree line

#d)
llmax1 <- fit.week$llmax
h.func<-function(M){1/sig.week*exp(-(M-mu.week)/sig.week - exp(-(M-mu.week)/sig.week))}
llmax0 <- sum(log(h.func(max_array)))
D<- 2*(llmax1 - llmax0)
D
1-pchisq(D,1)

#e)
MEplot(x_array, main = "Sample Mean-Excess Plot")
quantile(x_array,0.9)
MEplot(x_array, main = "Sample Mean-Excess Plot", xlim = c(5,15), ylim = c(0,10)) #zoomin
#pick d1 = 6
d1 <- 6
abline(v = d1, col = 2)

#f)
fitGPD <- fit.GPD(x_array, threshold = d1)

xi2 <- fitGPD$par.ests[["xi"]]
beta2 <- fitGPD$par.ests[["beta"]]
lmax1 <- fitGPD$ll.max

#g)
g.func<-function(X){1/beta2 * exp(-X/beta2)}
lmax0 <- sum(log(g.func(x_array[x_array>d1] - d1)))
lmax1
lmax0
D2<- 2*(lmax1 - lmax0)
1-pchisq(D2,1)

#h)
Sxd <- length(x_array[x_array>d1])/length(x_array)
Q99<- d1 + beta2/xi2 * ((Sxd / (1 - 0.99))^xi2 - 1)
Q995<- d1 + beta2/xi2 * ((Sxd / (1 - 0.995))^xi2 - 1)
Q99
Q995
CTE99 <- (Q99 + beta2 - xi2*d1)/(1-xi2)
CTE995 <- (Q995 + beta2 - xi2*d1)/(1-xi2)
CTE99
CTE995

#empirical
Q99e <- quantile(x_array,0.99)
Q995e <- quantile(x_array,0.995)
Q99e
Q995e

CTE99e <-mean(x_array[x_array > Q99e])
CTE995e <- mean(x_array[x_array > Q995e])

CTE99e
CTE995e