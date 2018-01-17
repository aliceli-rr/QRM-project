rm(list=ls())
library("tseries", lib.loc="~/R/win-library/3.4")
library("Kendall")
library("QRM")
library("EVD")
library("copula")


#a)
RY<-read.csv("RY.csv")
MS<-read.csv("MS.csv")
a_array <- rep(0,nrow(RY)-1)
b_array <- rep(0,nrow(MS)-1)

for (i in 1:nrow(RY)-1){
  a_array[i] = -100*log(RY$Adj.Close[i+1]/RY$Adj.Close[i])
}

for (i in 1:nrow(MS)-1){
  b_array[i] = -100*log(MS$Adj.Close[i+1]/MS$Adj.Close[i])
}

plot.ts(a_array,ylab="RY log return")
plot.ts(b_array,ylab="MS log return")

#b)
#install package tseries
jarque.bera.test(a_array)
jarque.bera.test(b_array)
#Rejected 

#c)
plot(a_array, b_array, main="Scatterplot between Stock A and Stock B", 
     xlab="Royal Bank Log Return", ylab="Morgan Stanley Log Return")


#d)
# ranka_array<-rep(0,length(a_array))
# for(ii in 1:length(a_array)){
#   ranka_array[ii]<-which(sort(a_array)==a_array[ii])/(length(a_array)+1)
# }
# 
# rankb_array<-rep(0,length(b_array))
# for(ii in 1:length(b_array)){
#   rankb_array[ii]<-which(sort(b_array)==b_array[ii])/(length(b_array)+1)
# }
# plot(ranka_array, rankb_array, main="Scatterplot Of Probability Transformed Pairs Of Stock A and B", 
#      xlab="Royal Bank Log Return", ylab="Morgan Stanley Log Return")



#4)
ranka_array<-rank(a_array, ties.method = 'random')
rankb_array<-rank(b_array, ties.method = 'random')
n<-length(a_array)
ra<-ranka_array/(n+1)
rb<-rankb_array/(n+1)

plot(ra, rb, main="Scatterplot Of Rank Of Stock A and B", 
     xlab="Royal Bank Log Return", ylab="Morgan Stanley Log Return")

#5)
#a)
Kendall(ra,rb)
rab<-cbind(ra,rb)
#Upper right conner of top 70%
UR70<-rab[ra>0.7 & rb>0.7,]
Kendall(UR70[,1],UR70[,2])

#Upper right conner of top 80%
UR80<-rab[ra>0.8 & rb>0.8,]
Kendall(UR80[,1],UR80[,2])

#Upper right conner of top 95%
UR95<-rab[ra>0.95 & rb>0.95,]
Kendall(UR95[,1],UR95[,2])

#Upper right conner of top 99%
UR99<-rab[ra>0.99 & rb>0.99,]
Kendall(UR99[,1],UR99[,2])

#Upper right conner of top 99.5%
UR995<-rab[ra>0.995 & rb>0.995,]
Kendall(UR995[,1],UR995[,2])



#Lower left conner of top 30%
LL30<-rab[ra<0.3 & rb<0.3,]
Kendall(LL30[,1],LL30[,2])

#Lower left conner of top 20%
LL20<-rab[ra<0.2 & rb<0.2,]
Kendall(LL20[,1],LL20[,2])

#Lower left conner of top 5%
LL5<-rab[ra<0.05 & rb<0.05,]
Kendall(LL5[,1],LL5[,2])

#Lower left conner of top 1%
LL1<-rab[ra<0.01 & rb<0.01,]
Kendall(LL1[,1],LL1[,2])

#Lower left conner of top 0.5%
LL05<-rab[ra<0.005 & rb<0.005,]
Kendall(LL05[,1],LL05[,2])


#b)
UTD95 <- length(rab[ra>0.95 & rb > 0.95])/length(rab[rb > 0.95])
UTD99 <- length(rab[ra>0.99 & rb > 0.99])/length(rab[rb > 0.99])
UTD995 <-length(rab[ra>0.995 & rb > 0.995,])/length(rab[rb > 0.995])

LTD5 <- length(rab[ra<0.05 & rb < 0.05])/length(rab[rb < 0.05])
LTD1 <- length(rab[ra<0.01 & rb < 0.01])/length(rab[rb < 0.01])
LTD05 <- length(rab[ra<0.005 & rb < 0.005])/length(rab[rb < 0.005])


UTD95a <- length(rab[ra>0.95 & rb > 0.95])/length(rab[ra > 0.95])
UTD99a <- length(rab[ra>0.99 & rb > 0.99])/length(rab[ra > 0.99])
UTD995a <-length(rab[ra>0.995 & rb > 0.995,])/length(rab[ra > 0.995])

LTD5a <- length(rab[ra<0.05 & rb < 0.05])/length(rab[ra < 0.05])
LTD1a <- length(rab[ra<0.01 & rb < 0.01])/length(rab[ra < 0.01])
LTD05a <- length(rab[ra<0.005 & rb < 0.005])/length(rab[ra < 0.005])


#c)

#6
#Gumbel
gumfit <- fit.AC(rab, name = "gumbel")
#Gaussian
gausfit <- fit.gausscopula(rab)
#student t
tfit <- fit.tcopula(rab)





