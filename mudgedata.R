# EDA for the Italian data

library(maps)
library(mapdata)
library(soap)
library(maps)
library(adehabitat)

# extra scripts
source("pe.R")
source("latlong2km.R")
source("makesoapgrid.R")
source("eda.R")
source("models.R")

# load full data
full<-read.csv(file="database/database_complete.csv")

fixdat<-fix_it_data(full)


# just for italy at the moment

#fullll<-data.frame(lat=c(fixdat$italy$dat$lat),
#                   long=c(fixdat$italy$dat$long),
#                   share_100=c(fixdat$italy$dat$share_100),
#                   altimetry=c(fixdat$italy$dat$altimetry),
#                   year=c(fixdat$italy$dat$year))

fullll<-data.frame(lat= c(fixdat$italy$dat$lat,
                           fixdat$sicily$dat$lat,
                           fixdat$sardinia$dat$lat),
                   long=c(fixdat$italy$dat$long,
                          fixdat$sicily$dat$long,
                          fixdat$sardinia$dat$long),
                   share_100=c(fixdat$italy$dat$share_100,
                               fixdat$sicily$dat$share_100,
                               fixdat$sardinia$dat$share_100),
                   year=c(fixdat$italy$dat$year,
                          fixdat$sicily$dat$year,
                          fixdat$sardinia$dat$year))


plot.it<-TRUE
zlim<-c(0,12)
year<-""
par(mfrow=c(2,3))

# need to create a new data frame which has the same data but averaged
# over a lower resolution

### build a grid   
# use adehabitat to make the grid for us...
grid.res<-100

# convert latlong to eastings and northings, put them in the data frame
ne.km<-latlong2km(fullll$long,fullll$lat,11.5,44)
names(ne.km)<-c("x","y")
fullll$x<-ne.km$x
fullll$y<-ne.km$y

# remove long and lat
fullll$long<-NULL
fullll$lat<-NULL


# make a matrix of the latitudes and longitudes
itmat<-matrix(c(ne.km$x,ne.km$y),length(ne.km$x),2)
it.asc<-ascgen(itmat,nrcol=grid.res)

# now extract the grid
gridcuts<-attr(it.asc,"dimnames")
gridcuts$x<-gsub("\\(","",gridcuts$x)
gridcuts$x<-gsub("\\]","",gridcuts$x)
gridcuts$y<-gsub("\\(","",gridcuts$y)
gridcuts$y<-gsub("\\]","",gridcuts$y)

gridcuts$x<-t(matrix(as.numeric(unlist(strsplit(gridcuts$x,",",extended=TRUE)),2,grid.res),2,grid.res))
gridcuts$y<-t(matrix(as.numeric(unlist(strsplit(gridcuts$y,",",extended=TRUE)),2,grid.res),2,grid.res))

x.start<-gridcuts$x[,1]
x.stop <-gridcuts$x[,2]
y.start<-gridcuts$y[,1]
y.stop <-gridcuts$y[,2]
   
# build the data frame to keep everything in
av.dat<-list(x=c(),y=c(),share_100=c(),year=c())


years<-as.numeric(levels(as.factor(fullll$year)))

# first loop over years
for(year in years){
   ydat<-pe(fullll,fullll$year==year)

   # image matrix
   im.mat<-matrix(NA,grid.res,grid.res)
   
   # put the observations into the grid
   for(i in 1:length(x.start)){
      for(j in 1:length(y.start)){
         ind<-ydat$y>=y.start[j] & ydat$y<y.stop[j] &
                    ydat$x>=x.start[i] & ydat$x<x.stop[i]
   
         # take the mean of the standardised proportion of foreign
         # population, ignoring NAs
         sq<-mean(ydat$share_100[ind],na.rm=T)
         if(is.nan(sq)) sq<-0
   
         # put the entry in the matrix (for checking)
         im.mat[i,j]<-sq
   
         # store the midpoints of the grid in the dataframe
         av.dat$x<-c(av.dat$x,(x.start[i]+x.stop[i])/2)
         av.dat$y<-c(av.dat$y,(y.start[j]+y.stop[j])/2)
         
         # year
         av.dat$year<-c(av.dat$year,year)
      
         # average share_100 value
         av.dat$share_100<-c(av.dat$share_100,sq)
      }
   }

   im.copy<-im.mat
   im.copy[is.nan(im.copy)]<-0
   im.copy[is.na(im.copy)]<-0
   seqlen<-sum(rowSums(im.copy)>0)
   
   im.copy[im.copy==0]<-NA
   
   if(plot.it){
      # plot with map overlay
      image(z=im.copy,#x=xs,y=ys,
            col=heat.colors(100),xlab="km (e)",ylab="km (n)",
            main=paste("Raw data",year),asp=1,zlim=zlim,cex.main=1.4,
            cex.lab=1.4,cex.axis=1.3)#,xlim=xlim,ylim=ylim)
      lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)
      lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)
      lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)
   }

}

