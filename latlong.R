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


for(i in 130:n.places){

   this.place<-placedat$Comune[i]

   if(grep("è",this.place)>0){
      this.place<-gsub("è","%E8",this.place)
   }

   q<-GNsearch(q=this.place,maxRows=1,style="SHORT",
               warn=FALSE,country="IT",featureCode="ADM3")
   lat[i]<-q$lat
   long[i]<-q$lng

   if(i%%100==0){
      Sys.sleep(30)
   }
   
}




