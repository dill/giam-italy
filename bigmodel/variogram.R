# plot the variogram for the Italian data
# want to test for spatial auto-correlation

# need geoR
library(geoR)
library(soap)

load("fullmod-Gamma.RData")


make_variogram<-function(model,fitres=FALSE){

   # taken from the Red Book
   coords<-matrix(0,length(model$model$x),2)
   coords[,1]<-model$model$x
   coords[,2]<-model$model$y


   gb<-list(data=residuals(model,type="d"),coords=coords)
#   vg<-variog(gb,max.dist=1200)
   vg<-variog(gb,max.dist=100)
   vg.env<-variog.mc.env(gb, obj.var = vg,nsim=99)

   # plot the fitted vs residuals too?
   if(fitres){
      par(mfrow=c(1,2))
      plot(fitted(model),residuals(model))
   }


   # plot the variogram
   plot(vg,envelope=vg.env,type="l")

}

pdf(file="variogram.pdf",width=8,height=4)
make_variogram(it.soap,fitres=TRUE)
dev.off()

save.image("variogram.RData")


