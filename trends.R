# do the temporal trends plot...


set.seed(1)

library(mgcv)
library(soap)
library(mvtnorm)

# model stuff first...
load("REMLfull.RData")

######################################

# trends stuff from here down...

# options for mvnorm
sig.lev <- 0.05
n.rep   <- 1000
s.meth  <- "svd"

b<-it.soap
# extract the model matrix
Xb<-model.matrix(b)

# Nb - number of times we generate
Nb<-200

# create the indicators
north <-(av.dat.it$y > -20)
centre<-(av.dat.it$y < -20 & av.dat.it$y > -300)
south <-(av.dat.it$y < -300)

year<-list()

year[[1]]<-av.dat.it$year==2003
year[[2]]<-av.dat.it$year==2004
year[[3]]<-av.dat.it$year==2005
year[[4]]<-av.dat.it$year==2006
year[[5]]<-av.dat.it$year==2007
year[[6]]<-av.dat.it$year==2008

# results matrix
#RES<-matrix(NA,dim(Xb)[1],Nb)
RES<-matrix(NA,24,Nb)
#RES<-rep(NA,Nb)

for( i in 1:Nb){
   # simulate \theta_b
   bs  <- rmvnorm(1, mean = b$coeff, sigma=b$Vp, method=s.meth)
   # calculate the expected value
   ex<-exp(Xb%*%t(bs))

   for(j in 1:6){
      RES[j,i]<-mean(ex[year[[j]],])
      RES[j+6,i]<-mean(ex[north & year[[j]],])
      RES[j+12,i]<-mean(ex[centre & year[[j]],])
      RES[j+18,i]<-mean(ex[south & year[[j]],])
   }

}


save.image(file="trend.RData")

par(mfrow=c(2,2))

titles<-c("Mainland Italy","North","Centre","South")

j<-1

for(i in c(0,6,12,18)){
   plot(apply(RES[(1+i):(6+i),],1,median),type="l",x=seq(2003,2008,1),
        ylab="Incidence",xlab="Year",main=titles[j],ylim=c(0,7))
   lines(apply(RES[(1+i):(6+i),],1,quantile,sig.lev/2),lty=2,x=seq(2003,2008,1))
   lines(apply(RES[(1+i):(6+i),],1,quantile,1-sig.lev/2),lty=3,x=seq(2003,2008,1))
   j<-j+1
}


