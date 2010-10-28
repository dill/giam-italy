# diagnostic code
# creates a 2x2 plot of 
# * boxplot spatial residuals
# * boxplot temporal residuals
# * qqplot
# * scale-location plot

# boxplots are at resolution res
# resid.type residuals are used
diagnostic<-function(model,res=20,resid.type="deviance"){
   # plot settings here
   par(mfrow=c(2,2),pch=19,mar=c(3,3,2,1.5),cex.axis=0.7,mgp=c(1.75,1,0),cex.main=0.95)

   ### boxplots
   # first generate the grid...
   it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)
   xmax<-max(it$x)
   ymax<-max(it$y)
   xmin<-min(it$x)
   ymin<-min(it$y)
   delx<-(xmax-xmin)/res
   dely<-(ymax-ymin)/res
   
   # grab the residuals
   resids<-residuals(model,type=resid.type)
   
   # find the grid cells the residuals lie in...
   xi<-abs(floor((it.dat$x-xmin)/delx))#+1
   yj<-abs(floor((it.dat$y-ymin)/dely))#+1
   
   ### year data
   yeardata<-data.frame(ind=it.dat$year,resids=resids)
   boxplot(resids~ind,data=yeardata,main="Annual distribution of residuals",
            cex=0.3,las=1,xaxt="n",
            xlab="year",ylab="Residuals")
   axis(1,at=1:6,labels=c("03","04","05","06","07","08"))
   
   ### box index...
   boxind<-data.frame(ind=yj*res+xi,resids=resids)
   boxplot(resids~ind,data=boxind,main="Spatial aggregation index",
            xlab="index",ylab="Residuals",
            cex=0.3,xaxt="n",las=1)
   axis(1,at=1:length(unique(boxind$ind)))

   ### Normal qqplot
   qqnorm(residuals(model,type=resid.type),cex=0.3,asp=1,las=1)
   abline(0,1)

   ### scale-location plot
   sl.dat<-list(x=fitted(model),y=abs(residuals(model,type=resid.type)))
#   plot(x=c(0.5,14),y=c(1.1,5),type="n",las=1,asp=1,
#        main="Scale-location plot",
#        ylab="Abs. value of residuals",
#        xlab="Predicted values",cex=0.3)
#   points(sl.dat,cex=0.3)
   plot(sl.dat,las=1,
        main="Scale-location plot",
        ylab="Abs. value of residuals",
        xlab="Predicted values",cex=0.3)

   #loess fit..
   loe<-loess(y~x,data=sl.dat)
   nd<-seq(min(sl.dat$x,na.rm=T),max(sl.dat$x,na.rm=T))
   pred<-predict(loe,newdata=nd,by=0.01) 
   lines(nd,pred,col="grey")

}
