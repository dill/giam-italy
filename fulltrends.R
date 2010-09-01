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

# Italy model
b<-it.soap
# extract the model matrix
Xb<-model.matrix(b)

# Sicily model
b.sc<-sc.soap
# extract the model matrix
Xb.sc<-model.matrix(b.sc)

# Sardinia model
b.sa<-sa.soap
# extract the model matrix
Xb.sa<-model.matrix(b.sa)

# Nb - number of times we generate
Nb<-200

# create the indicators
north <-(av.dat.it$y > -20)
centre<-(av.dat.it$y < -20 & av.dat.it$y > -300)
south <-(av.dat.it$y < -300)

# Italy
year<-list()
year[[1]]<-av.dat.it$year==2003
year[[2]]<-av.dat.it$year==2004
year[[3]]<-av.dat.it$year==2005
year[[4]]<-av.dat.it$year==2006
year[[5]]<-av.dat.it$year==2007
year[[6]]<-av.dat.it$year==2008


# Sardinia
sa.year<-list()
sa.year[[1]]<-av.dat.sa$year==2003
sa.year[[2]]<-av.dat.sa$year==2004
sa.year[[3]]<-av.dat.sa$year==2005
sa.year[[4]]<-av.dat.sa$year==2006
sa.year[[5]]<-av.dat.sa$year==2007
sa.year[[6]]<-av.dat.sa$year==2008

# Sicily
sc.year<-list()
sc.year[[1]]<-av.dat.sc$year==2003
sc.year[[2]]<-av.dat.sc$year==2004
sc.year[[3]]<-av.dat.sc$year==2005
sc.year[[4]]<-av.dat.sc$year==2006
sc.year[[5]]<-av.dat.sc$year==2007
sc.year[[6]]<-av.dat.sc$year==2008

# results matrix
#RES<-matrix(NA,dim(Xb)[1],Nb)
RES<-matrix(NA,24,Nb)
#RES<-rep(NA,Nb)

for( i in 1:Nb){
   ## Italy
   # simulate \theta_b
   bs  <- rmvnorm(1, mean = b$coeff, sigma=b$Vp, method=s.meth)
   # calculate the expected value
   ex<-exp(Xb%*%t(bs))

   ## Sicily
   # simulate \theta_b
   bs.sc  <- rmvnorm(1, mean = b.sc$coeff, sigma=b.sc$Vp, method=s.meth)
   # calculate the expected value
   ex.sc<-exp(Xb.sc%*%t(bs.sc))

   ## Sardinia
   # simulate \theta_b
   bs.sa  <- rmvnorm(1, mean = b.sa$coeff, sigma=b.sa$Vp, method=s.meth)
   # calculate the expected value
   ex.sa<-exp(Xb.sa%*%t(bs.sa))


   for(j in 1:6){
      RES[j,i]<-mean(c(ex[year[[j]],],ex.sa[sa.year[[j]],],ex.sc[sc.year[[j]],]))
      RES[j+6,i]<-mean(ex[north & year[[j]],])
      RES[j+12,i]<-mean(ex[centre & year[[j]],])
      RES[j+18,i]<-mean(c(ex[south & year[[j]],],ex.sa[sa.year[[j]],],ex.sc[sc.year[[j]],]))
   }

}


save.image(file="fulltrend.RData")

#par(mfrow=c(2,2),las=1,mar=c(3.5,3,2,0.75),mgp=c(2,0.65,0))
#
#titles<-c("Italy","North","Centre","South")
#
#j<-1
#
#for(i in c(0,6,12,18)){
#   plot(apply(RES[(1+i):(6+i),],1,median),type="l",x=seq(2003,2008,1),
#        ylab="Incidence",xlab="Year",main=titles[j],ylim=c(0,7))
#   lines(apply(RES[(1+i):(6+i),],1,quantile,sig.lev/2),lty=2,x=seq(2003,2008,1))
#   lines(apply(RES[(1+i):(6+i),],1,quantile,1-sig.lev/2),lty=3,x=seq(2003,2008,1))
#   j<-j+1
#}


