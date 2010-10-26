# diagnostic code

diagnostic<-function(model,resid.type="deviance"){
   # plot settings here
   par(mfrow=c(2,2),pch=19)

   # resids vs. x
   plot(residuals(model,type=resid.type),it.dat$x,
        main=paste(resid.type," resids vs. x",sep=""),
        xlab=paste(resid.type," residuals",sep=""),ylab="x",cex=0.3)

   # resids vs. y
   plot(residuals(model,type=resid.type),it.dat$y,
        main=paste(resid.type," resids vs. y",sep=""),
        xlab=paste(resid.type," residuals",sep=""),ylab="y",cex=0.3)

   # resids vs. t
   plot(residuals(model,type=resid.type),it.dat$year,
        main=paste(resid.type," resids vs. year",sep=""),
        xlab=paste(resid.type," residuals",sep=""),ylab="year",cex=0.3)

   # scale-location plot
   plot(fitted(model),abs(residuals(model,type=resid.type)),
        main="scale-location plot",
        xlab=paste(resid.type," residuals",sep=""),
        ylab="fitted values",cex=0.3)


}

