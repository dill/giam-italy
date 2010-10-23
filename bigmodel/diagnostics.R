# diagnostic code

diagnostic<-function(model){

   # plot settings here
   par(mfrow=c(2,2),pch=19)

   # resids vs. x
   plot(residuals(model,type="deviance"),it.dat$x,
        main="Deviance resids vs. x",
        xlab="Deviance residuals",ylab="x",cex=0.3)

   # resids vs. y
   plot(residuals(model,type="deviance"),it.dat$y,
        main="Deviance resids vs. y",
        xlab="Deviance residuals",ylab="y",cex=0.3)

   # resids vs. t
   plot(residuals(model,type="deviance"),it.dat$year,
        main="Deviance resids vs. year",
        xlab="Deviance residuals",ylab="year",cex=0.3)

   # scale-location plot
   plot(abs(residuals(model,type="deviance")),fitted(model),
        main="scale-location plot",
        xlab="Deviance residuals",ylab="fitted values",cex=0.3)


}

