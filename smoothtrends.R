# do the temporal trends plot...
# but this time make it smooth...


library(mgcv)
library(soap)
library(mvtnorm)
require(splines)

# model stuff first...
load("REMLfull.RData")

# load the trend data...
load("trend.RData")


######### put the Sardinia and Sicily stuff in

# Sardinia
set.seed(1)

year[[1]]<-av.dat.sa$year==2003
year[[2]]<-av.dat.sa$year==2004
year[[3]]<-av.dat.sa$year==2005
year[[4]]<-av.dat.sa$year==2006
year[[5]]<-av.dat.sa$year==2007
year[[6]]<-av.dat.sa$year==2008

b<-sa.soap
# extract the model matrix
Xb<-model.matrix(b)

for( i in 1:Nb){
   # simulate \theta_b
   bs  <- rmvnorm(1, mean = b$coeff, sigma=b$Vp, method=s.meth)
   # calculate the expected value
   ex<-exp(Xb%*%t(bs))

   for(j in 1:6){
      # full
      RES[j,i]<-mean(c(RES[j,i],ex[year[[j]],]))
      # south
      RES[j+18,i]<-mean(c(RES[j+18,i],ex[year[[j]],]))
   }
}

# Sicily
set.seed(1)

year[[1]]<-av.dat.sc$year==2003
year[[2]]<-av.dat.sc$year==2004
year[[3]]<-av.dat.sc$year==2005
year[[4]]<-av.dat.sc$year==2006
year[[5]]<-av.dat.sc$year==2007
year[[6]]<-av.dat.sc$year==2008

b<-sc.soap
# extract the model matrix
Xb<-model.matrix(b)

for( i in 1:Nb){
   # simulate \theta_b
   bs  <- rmvnorm(1, mean = b$coeff, sigma=b$Vp, method=s.meth)
   # calculate the expected value
   ex<-exp(Xb%*%t(bs))

   for(j in 1:6){
      # full
      RES[j,i]<-mean(c(RES[j,i],ex[year[[j]],]))
      # south
      RES[j+18,i]<-mean(c(RES[j+18,i],ex[year[[j]],]))
   }
}




##############################################

# plot stuff
par(mfrow=c(2,2),las=1,mar=c(3.5,3,2,0.75),mgp=c(2,0.65,0))

titles<-c("Mainland Italy","North","Centre","South")

j<-1

for(i in c(0,6,12,18)){

   # years
   yy<-seq(2003,2008,1)

   # smooth for median
   med.line<-predict(interpSpline(yy,apply(RES[(1+i):(6+i),],1,median)))

   # upper CI
   u.line<-predict(interpSpline(yy,apply(RES[(1+i):(6+i),],1,quantile,sig.lev/2)))

   # lower CI
   l.line<-predict(interpSpline(yy,apply(RES[(1+i):(6+i),],1,quantile,1-sig.lev/2)))

   plot(med.line$y,type="l",x=med.line$x,
        ylab="Incidence",xlab="Year",main=titles[j],ylim=c(0,7))
   lines(u.line$y,lty=2,x=u.line$x)
   lines(l.line$y,lty=3,x=l.line$x)
   j<-j+1
}




