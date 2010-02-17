# plot the variogram for the Italian data
# want to test for spatial auto-correlation

# need geoR
library(geoR)


make_variogram<-function(model,fitres=FALSE){


   # taken from the Red Book
   coords<-matrix(0,n,2)
   coords[,1]<-
   coords[,2]<-


   gb<-list(data=residuals(model,type="d"),coords=coords)


   # plot the fitted vs residuals too?
   if(fitres){
      par(mfrow=c(1,2))
      plot(fitted(model),residuals(model))
   }

   # plot the variogram
   plot(variog(gb,max.dist=1e5))

}

