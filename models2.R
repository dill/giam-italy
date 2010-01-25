# try fitting some models to the Italian data...
# now using Northings and Eastings...

source("pe.R")

# pars
par(mfrow=c(2,2))

# first source in the eda file, to aggregate the data for us...
source("eda.R")
source("latlong2km.R")

image.asc(it.asc,main="observation density")


# load mgcv
library(mgcv)

# now create the data set to pass to the GAM

# duplicate the lats and longs
llats <- rep(lats,length(longs))
llongs<-rep(longs,rep(length(lats),length(longs)))

im.mat[is.nan(im.mat)]<-NA

ind<-!is.na(im.mat)

it.dat<-data.frame(lat=llats[ind],
                   long=llongs[ind],
                   z=im.mat[ind])

b.it<-gam(z~s(lat,long,k=20),data=it.dat)


# in kilometres
kms<-latlong2km(it.dat$lat,it.dat$long,min(it.dat$lat),min(it.dat$long))
it.dat.km<-data.frame(x=kms$km.e,
                   y=kms$km.n,
                   z=it.dat$z)

b.it.km<-gam(z~s(x,y,k=20),data=it.dat.km)




# plot to compare

par(mfrow=c(1,2))

vis.gam(b.it,plot.type="contour",asp=1,contour.col=rev(heat.colors(100)))
map('italy',add=TRUE)


vis.gam(b.it.km,plot.type="contour",asp=1,contour.col=rev(heat.colors(100)))
map('italy',add=TRUE)



# now let's just look at the mainland

# construct the boundary
library(mapdata)
it.map<-map('world2',regions='Italy',plot=FALSE,exact=TRUE,boundary=TRUE)

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

it.map.km<-latlong2km(it.map$x,it.map$y,min(it.dat$lat),min(it.dat$long))
lines(it.map.km$km.e, it.map.km$km.n)

#
## find the mainland
#library(soap)
#onoff<-inSide(it.map,it.dat$lat,it.dat$long)
#
#it.dat<-pe(it.dat,onoff)
#
#b.it<-gam(z~s(lat,k=50)+s(long,k=50),data=it.dat)
#
#vis.gam(b.it,plot.type="contour",asp=1,contour.col=rev(heat.colors(100)))
#lines(it.map)
#
#
