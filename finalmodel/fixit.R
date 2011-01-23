# using the new database now...

# packages
library(maps)
library(mapdata)
library(soap)

# function to do the formatting to the data
fix_it_data<-function(data){

   ##### Mainland
   # make a copy of the data
   it.dat<-data
   # first grab the shape of Italy from the mapdata package
   it.map<-map('worldHires',regions='Italy',plot=FALSE,exact=TRUE,boundary=TRUE)

   # using inSide from soap, get rid of the non-mainland points
   # islands are separated from the mainland by NAs
   ind<-c(1:length(it.map$x))
   ind[is.na(it.map$x)]<-NA
   
   end<-c(which(is.na(ind))-1,length(ind))
   start<-c(1,which(is.na(ind))+1)
   
   for(i in 1:length(start)){
      it.map$x[start[i]:end[i]]<-rev(it.map$x[start[i]:end[i]])
      it.map$y[start[i]:end[i]]<-rev(it.map$y[start[i]:end[i]])
   }
   
   it.map<-list(x=c(it.map$x[!is.na(it.map$x)],it.map$x[1]),
                y=c(it.map$y[!is.na(it.map$x)],it.map$y[1]))

   # simplify the boundary
   it.map<-pe(it.map,c(seq(1,length(it.map$x),100),1))
   
   # find the mainland
   onoff<-inSide(it.map,it.dat$longitude,it.dat$latitude)
   it.dat<-pe(it.dat,onoff)

   # now it.dat contains only the points on the mainland

   # convert to northings and eastings
   # km relative to 11.5,44 (approx centroid)
   ne.km<-latlong2km(it.dat$longitude,it.dat$latitude,11.5,44)

   # add the northings and eastings into the dataframe 
   it.dat$km.e<-ne.km$km.e
   it.dat$km.n<-ne.km$km.n

   # also return the boundary in km
   it.map<-latlong2km(it.map$x,it.map$y,11.5,44)

   ### Sicily
   sc.dat<-data
   sc.map<-map('worldHires',regions='Sicily',plot=FALSE,exact=TRUE,boundary=TRUE)
   # simplify the boundary
   sc.map<-pe(sc.map,c(seq(1,length(sc.map$x),50),1))

   ind<-c(1:length(sc.map$x))
   onoff<-inSide(sc.map,sc.dat$longitude,sc.dat$latitude)
   sc.dat<-pe(sc.dat,onoff)
   ne.km<-latlong2km(sc.dat$longitude,sc.dat$latitude,11.5,44)

   # add the northings and eastings into the dataframe 
   sc.dat$km.e<-ne.km$km.e
   sc.dat$km.n<-ne.km$km.n

   # also return the boundary in km
   sc.map<-latlong2km(sc.map$x,sc.map$y,11.5,44)

   ### Sardinia
   sa.dat<-data
   sa.map<-map('worldHires',regions='Sardinia',plot=FALSE,exact=TRUE,boundary=TRUE)
   sa.map<-pe(sa.map,c(seq(1,length(sa.map$x),50),1))

   ind<-c(1:length(sa.map$x))
   onoff<-inSide(sa.map,sa.dat$longitude,sa.dat$latitude)
   sa.dat<-pe(sa.dat,onoff)

   # add the northings and eastings into the dataframe 
   ne.km<-latlong2km(sa.dat$longitude,sa.dat$latitude,11.5,44)
   sa.dat$km.e<-ne.km$km.e
   sa.dat$km.n<-ne.km$km.n

   # also return the boundary in km
   sa.map<-latlong2km(sa.map$x,sa.map$y,11.5,44)

   return(list(italy=list(dat=it.dat,map=it.map),
               sicily=list(dat=sc.dat,map=sc.map),
               sardinia=list(dat=sa.dat,map=sa.map)))
} 
