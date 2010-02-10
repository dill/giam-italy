# using the new database now...

# packages
library(maps)
library(mapdata)
library(soap)

# extra scripts
source("pe.R")
source("latlong2km.R")

# first read in the csv for 2003 and 2008 for the whole of italy
it2003<-read.csv(file="database/database_2003.csv")


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

   # make share_100 work with the log link
   it.dat$share_100<-it.dat$share_100+1e-6

   # also return the boundary in km
   it.map<-latlong2km(it.map$x,it.map$y,11.5,44)

   ### Sicily
   sc.dat<-data
   sc.map<-map('worldHires',regions='Sicily',plot=FALSE,exact=TRUE,boundary=TRUE)
   ind<-c(1:length(sc.map$x))
   onoff<-inSide(sc.map,sc.dat$longitude,sc.dat$latitude)
   sc.dat<-pe(sc.dat,onoff)
   ne.km<-latlong2km(sc.dat$longitude,sc.dat$latitude,11.5,44)

   # add the northings and eastings into the dataframe 
   sc.dat$km.e<-ne.km$km.e
   sc.dat$km.n<-ne.km$km.n

   # make share_100 work with the log link
   sc.dat$share_100<-sc.dat$share_100+1e-6

   # also return the boundary in km
   sc.map<-latlong2km(sc.map$x,sc.map$y,11.5,44)

   ### Sardinia
   sa.dat<-data
   sa.map<-map('worldHires',regions='Sardinia',plot=FALSE,exact=TRUE,boundary=TRUE)
   ind<-c(1:length(sa.map$x))
   onoff<-inSide(sa.map,sa.dat$longitude,sa.dat$latitude)
   sa.dat<-pe(sa.dat,onoff)

   # add the northings and eastings into the dataframe 
   ne.km<-latlong2km(sa.dat$longitude,sa.dat$latitude,11.5,44)
   sa.dat$km.e<-ne.km$km.e
   sa.dat$km.n<-ne.km$km.n

   # make share_100 work with the log link
   sa.dat$share_100<-sa.dat$share_100+1e-6

   # also return the boundary in km
   sa.map<-latlong2km(sa.map$x,sa.map$y,11.5,44)

   return(list(italy=list(dat=it.dat,map=it.map),
               sicily=list(dat=sc.dat,map=sc.map),
               sardinia=list(dat=sa.dat,map=sa.map)))
} 


# run the data formatter for the 2003 data set
fixdat<-fix_it_data(it2003)

# run the eda file first sticking all the data together
fullll<-data.frame(lat= c(fixdat$italy$dat$latitude,
                           fixdat$sicily$dat$latitude,
                           fixdat$sardinia$dat$latitude),
                    long=c(fixdat$italy$dat$longitude,
                           fixdat$sicily$dat$longitude,
                           fixdat$sardinia$dat$longitude),
                    share_100=c(fixdat$italy$dat$share_100,
                                fixdat$sicily$dat$share_100,
                                fixdat$sardinia$dat$share_100))

fulldat<-data.frame(km.e=c(fixdat$italy$dat$km.e,
                           fixdat$sicily$dat$km.e,
                           fixdat$sardinia$dat$km.e),
                    km.n=c(fixdat$italy$dat$km.n,
                           fixdat$sicily$dat$km.n,
                           fixdat$sardinia$dat$km.n),
                    share_100=c(fixdat$italy$dat$share_100,
                                fixdat$sicily$dat$share_100,
                                fixdat$sardinia$dat$share_100))


# fit the models

# first the full model (italy+sardinia+sicily)
full.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fulldat)

# italy
it.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fixdat$italy$dat)
# sicily
sc.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fixdat$sicily$dat)
# sardinia
sa.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fixdat$sardinia$dat)

# time for some plots
par(mfrow=c(2,3))

# plot the raw data
source("eda.R")
n.grid<-100

vis.gam(full.b,plot.type="contour",n.grid=n.grid,contour.col=rev(heat.colors(100)),too.far=0.05,type="response",asp=1)
lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)
lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)
lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)

vis.gam(it.b,plot.type="contour",n.grid=n.grid,contour.col=rev(heat.colors(100)),too.far=0.01,type="response",asp=1)
lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)

vis.gam(sc.b,plot.type="contour",n.grid=n.grid,contour.col=rev(heat.colors(100)),too.far=0.1,type="response",asp=1)
lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)

vis.gam(sa.b,plot.type="contour",n.grid=n.grid,contour.col=rev(heat.colors(100)),too.far=0.1,type="response",asp=1)
lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)






