rm(list=ls())
#setwd("C:/Users/Tong/Desktop/UWaterloo/2017 Summer/ACTSC445/A4")
library("tseries")
library("Kendall")
library("QRM")
library("evd")
library("copula")

#1)
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

plot.ts(a_array,ylab="Log Return", main="Royal Bank Stock Log Return")
plot.ts(b_array,ylab="Log Return", main="Morgan Stanley Stock Log Return")

#2)
#install package tseries
jarque.bera.test(a_array)
jarque.bera.test(b_array)

#3)
plot(a_array, b_array, main="Scatterplot of Stock A and Stock B", 
     xlab="Royal Bank Log Return", ylab="Morgan Stanley Log Return", pch=19)

#4)
ranka_array<-rank(a_array, ties.method = 'random')
rankb_array<-rank(b_array, ties.method = 'random')
n<-length(a_array)
ra<-ranka_array/(n+1)
rb<-rankb_array/(n+1)

plot(ra, rb, main="Scatterplot of Stock A and B Rank", 
     xlab="Royal Bank Log Return Rank", ylab="Morgan Stanley Log Return Rank")

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
UTD95
UTD99
UTD995

LTD5 <- length(rab[ra<0.05 & rb < 0.05])/length(rab[rb < 0.05])
LTD1 <- length(rab[ra<0.01 & rb < 0.01])/length(rab[rb < 0.01])
LTD05 <- length(rab[ra<0.005 & rb < 0.005])/length(rab[rb < 0.005])
LTD5
LTD1
LTD05

UTD95a <- length(rab[ra>0.95 & rb > 0.95])/length(rab[ra > 0.95])
UTD99a <- length(rab[ra>0.99 & rb > 0.99])/length(rab[ra > 0.99])
UTD995a <-length(rab[ra>0.995 & rb > 0.995,])/length(rab[ra > 0.995])
UTD95a
UTD99a
UTD995a

LTD5a <- length(rab[ra<0.05 & rb < 0.05])/length(rab[ra < 0.05])
LTD1a <- length(rab[ra<0.01 & rb < 0.01])/length(rab[ra < 0.01])
LTD05a <- length(rab[ra<0.005 & rb < 0.005])/length(rab[ra < 0.005])
LTD5a
LTD1a
LTD05a

#c)

#6
#Gumbel 1
gumfit <- fit.AC(rab, name = "gumbel")
#Gaussian 1
gausfit <- fit.gausscopula(rab)
#student t 2
tfit <- fit.tcopula(rab)
gumfit
gausfit
tfit

gumllmax<-gumfit$ll.max
gausllmax<-gausfit$ll.max
tllmax<-tfit$ll.max

gumllmax
gausllmax
tllmax

gumAIC<- -2*gumllmax+2
gausAIC<- -2*gausllmax+2
tAIC<- -2*tllmax+4