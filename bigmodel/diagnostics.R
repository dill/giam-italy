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
   par(mfrow=c(2,2),pch=19,mar=c(4.5,4.5,2,2))

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
   boxplot(resids~ind,data=yeardata,main="year",cex=0.3,las=2)
   
   ### box index...
   boxind<-data.frame(ind=yj*res+xi,resids=resids)
   boxplot(resids~ind,data=boxind,main="box index",cex=0.3,xaxt="n")
   axis(1,at=1:length(unique(boxind$ind)))


   ### Normal qqplot
   qqnorm(residuals(model,type=resid.type),cex=0.3,asp=1)
   abline(0,1)

   ### scale-location plot
   plot(fitted(model),abs(residuals(model,type=resid.type)),
        main="scale-location",
        ylab=paste("Absolute value of ",resid.type," residuals",sep=""),
        xlab="Fitted values",cex=0.3)
}
