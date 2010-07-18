# do the temporal trends plot...

# but this time make it smooth...


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
# Nb - number of times we generate
Nb<-50

# temporal resolution
n.years<-10
years<-seq(2003,2008,len=n.years)

onoff<-inSide(it,pred.grid$x,pred.grid$y)

# prediction grid
pred.grid<-data.frame(x=rep(pred.grid$x[onoff],n.years),
                      y=rep(pred.grid$y[onoff],n.years),
                      year=years[rep(1:n.years,length(pred.grid$y[onoff]))]
                     )

# create the indicators
north <-(pred.grid$y > -20)[1:sum(onoff)]
centre<-(pred.grid$y < -20 & pred.grid$y > -300)[1:sum(onoff)]
south <-(pred.grid$y < -300)[1:sum(onoff)]

# results matrix
RES<-list(all=matrix(NA,n.years,Nb),
          north=matrix(NA,n.years,Nb),
          centre=matrix(NA,n.years,Nb),
          south=matrix(NA,n.years,Nb))


for( i in 1:Nb){
   # simulate \theta_b
   bs  <- rmvnorm(1, mean = b$coeff, sigma=b$Vp, method=s.meth)
   
   # insert the coefficicents into the object
   b$coefficients<-as.vector(bs)
   # do the prediction
   ex<-matrix(predict(b,pred.grid),n.years,length(pred.grid$year)/n.years)

   RES$all[,i]<-rowMeans(ex)
   RES$north[,i]<-rowMeans(ex[,north])
   RES$centre[,i]<-rowMeans(ex[,centre])
   RES$south[,i]<-rowMeans(ex[,south])
}


pdf(file="trends.pdf",width=4,height=4)
par(mfrow=c(2,2))


# all
plot(apply(RES$all,1,median),type="l",x=years,
     ylab="Incidence",xlab="Year",main="Mainland Italy",ylim=c(0,7))
lines(apply(RES$all,1,quantile,sig.lev/2),lty=2,x=years)
lines(apply(RES$all,1,quantile,1-sig.lev/2),lty=3,x=years)

# north
plot(apply(RES$north,1,median),type="l",x=years,
     ylab="Incidence",xlab="Year",main="North",ylim=c(0,7))
lines(apply(RES$north,1,quantile,sig.lev/2),lty=2,x=years)
lines(apply(RES$north,1,quantile,1-sig.lev/2),lty=3,x=years)

# centre
plot(apply(RES$centre,1,median),type="l",x=years,
     ylab="Incidence",xlab="Year",main="Centre",ylim=c(0,7))
lines(apply(RES$centre,1,quantile,sig.lev/2),lty=2,x=years)
lines(apply(RES$centre,1,quantile,1-sig.lev/2),lty=3,x=years)

# south
plot(apply(RES$south,1,median),type="l",x=years,
     ylab="Incidence",xlab="Year",main="South",ylim=c(0,7))
lines(apply(RES$south,1,quantile,sig.lev/2),lty=2,x=years)
lines(apply(RES$south,1,quantile,1-sig.lev/2),lty=3,x=years)

dev.off()
