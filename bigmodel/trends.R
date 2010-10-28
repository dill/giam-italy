# do the temporal trends plot...

set.seed(1)

library(mgcv)
library(soap)
library(mvtnorm)
library(splines)

# load the models 
#load("fullmod-Gamma.RData")# CHECK!
load("fullmod-Tweedie(1.2).RData") # load the data

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
Nb<-1000

# create the indicators
north <-(it.dat$y > -20)
centre<-(it.dat$y < -20 & it.dat$y > -300)
south <-(it.dat$y < -300)

# Italy
year<-list()
year[[1]]<-it.dat$year==2003
year[[2]]<-it.dat$year==2004
year[[3]]<-it.dat$year==2005
year[[4]]<-it.dat$year==2006
year[[5]]<-it.dat$year==2007
year[[6]]<-it.dat$year==2008


# Sardinia
sa.year<-list()
sa.year[[1]]<-sa.dat$year==2003
sa.year[[2]]<-sa.dat$year==2004
sa.year[[3]]<-sa.dat$year==2005
sa.year[[4]]<-sa.dat$year==2006
sa.year[[5]]<-sa.dat$year==2007
sa.year[[6]]<-sa.dat$year==2008

# Sicily
sc.year<-list()
sc.year[[1]]<-sc.dat$year==2003
sc.year[[2]]<-sc.dat$year==2004
sc.year[[3]]<-sc.dat$year==2005
sc.year[[4]]<-sc.dat$year==2006
sc.year[[5]]<-sc.dat$year==2007
sc.year[[6]]<-sc.dat$year==2008

# results matrix
RES<-matrix(NA,24,Nb)

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

# SAVE
save.image(file="trends.RData")



### Do the plotting

postscript(file="trends.ps",height=5,width=7)
par(mfrow=c(2,2),las=1,mar=c(3.5,3,2,0.75),mgp=c(2,0.65,0),cex=0.75,cex.lab=0.95)

titles<-c("Italy","North","Centre","South and Islands")

j<-1

for(i in c(0,6,12,18)){

   # years
   yy<-seq(2003,2008,1)

   ### smooths
   # smooth for median
   med.line<-predict(interpSpline(yy,apply(RES[(1+i):(6+i),],1,median)))
   # upper CI
   u.line<-predict(interpSpline(yy,apply(RES[(1+i):(6+i),],1,quantile,sig.lev/2)))
   # lower CI
   l.line<-predict(interpSpline(yy,apply(RES[(1+i):(6+i),],1,quantile,1-sig.lev/2)))

#   ### lines
#   #  median
#   med.line<-list(x=yy,y=apply(RES[(1+i):(6+i),],1,median))
#   # upper CI
#   u.line<-list(x=yy,y=apply(RES[(1+i):(6+i),],1,quantile,sig.lev/2))
#   # lower CI
#   l.line<-list(x=yy,y=apply(RES[(1+i):(6+i),],1,quantile,1-sig.lev/2))

   plot(med.line$y,type="l",x=med.line$x,
        ylab="Incidence",xlab="",main=titles[j],ylim=c(0,7))
   lines(u.line$y,lty=2,x=u.line$x)
   lines(l.line$y,lty=3,x=l.line$x)
   j<-j+1
}

dev.off()


