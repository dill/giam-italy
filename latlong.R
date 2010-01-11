# load some libraries
library(rjson)
library(geonames)

# load the data
placedat<-read.csv(file="italy.csv")

n.places<-length(placedat[,1])

lat<-rep(0,n.places)
long<-rep(0,n.places)


for(i in 1:n.places){

   q<-GNsearch(q=placedat$Comune[i],maxRows=1,style="SHORT",
               warn=FALSE,country="IT",featureCode="ADM3")
   lat[i]<-q$lat
   long[i]<-q$lng

   if(i%%100){
      Sys.sleep(30)
   }
   
}




