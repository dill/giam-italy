# do the temporal trends plot...
# but this time make it smooth...


library(mgcv)
library(soap)
library(mvtnorm)
require(splines)

# model stuff first...
load("REMLfull.RData")

# load the trend data...
#load("trend.RData")
load("fulltrend.RData")


##############################################

# plot stuff
par(mfrow=c(2,2),las=1,mar=c(3.5,3,2,0.75),mgp=c(2,0.65,0))

titles<-c("Italy","North","Centre","South")

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




