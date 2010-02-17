# plot the variogram for the Italian data
# want to test for spatial auto-correlation

# need geoR
library(geoR)

source("models.R")
# first read in the csv for 2003 and 2008 for the whole of italy
it2003<-read.csv(file="database/database_2003.csv")
it2008<-read.csv(file="database/database_2008.csv")

# run the models on both data sets 
fixdat<-fix_it_data(it2003)
mod_2003<-run_mods(fixdat,100,plot.it=FALSE)
fixdat<-fix_it_data(it2008)
mod_2008<-run_mods(fixdat,100,plot.it=FALSE)

make_variogram<-function(model,fitres=FALSE){

   # taken from the Red Book
   coords<-matrix(0,length(model$model$x),2)
   coords[,1]<-model$model$x
   coords[,2]<-model$model$y


   gb<-list(data=residuals(model,type="d"),coords=coords)


   # plot the fitted vs residuals too?
   if(fitres){
      par(mfrow=c(1,2))
      plot(fitted(model),residuals(model))
   }

   # plot the variogram
   plot(variog(gb,max.dist=1e8),type="l")

}

