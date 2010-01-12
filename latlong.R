# code to grab the latitude/longitude from the data Luca
# gave us. Uses geonames lookup. Sys.sleep is used to make
# sure that the website doesn't chuck us off.

# load some libraries
library(rjson)
library(geonames)

# load the data
placedat<-read.csv(file="italy.csv")

n.places<-length(placedat[,1])

lat<-rep(0,n.places)
long<-rep(0,n.places)

ind<-1:n.places
ind<-ind[-130]
ind<-ind[-225]

# error counter
err<-1

for(i in ind){

   this.place<-placedat$Comune[i]

   q<-GNsearch(q=this.place,maxRows=1,style="SHORT",
               warn=FALSE,country="IT",featureCode="ADM3")

   if(length(q)){
      lat[i]<-q$lat
      long[i]<-q$lng
   }else{
      cat(err,as.character(this.place),"\n")
      err<-err+1
   }

   # hourly limit is 5000 requests, so
   if((i %% 4000) == 0){
      Sys.sleep(3601)
   }

}




