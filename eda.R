# EDA for the Italian data

#### RUN FROM newdb.R ONLY!

# load the map package
library(maps)
# adehabitat for grids
library(adehabitat)

latlong<-data.frame(lat=fullll$lat,long=fullll$long)
placedat<-data.frame(stranieri_100=fullll$share_100)

# want to create a matrix to use with image

# first find the minimum distance between points in both
# directions

# use adehabitat to make the grid for us...
grid.res<-100
# make a matrix of the latitudes and longitudes
itmat<-matrix(c(latlong$long,latlong$lat),length(latlong$lat),2)
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

# image matrix
im.mat<-matrix(NA,grid.res,grid.res)


# put the observations into the grid
for(i in 1:length(x.start)){
   for(j in 1:length(y.start)){

      ind<-latlong$lat>=y.start[j] & latlong$lat<y.stop[j] &
                 latlong$long>=x.start[i] & latlong$long<x.stop[i]

      # take the mean of the standardised proportion of foreign
      # population, ignoring NAs
      sq<-mean(placedat$stranieri_100[ind],na.rm=T)

      im.mat[i,j]<-sq

   }
}


# create the grid sequences
lats<-seq(x.start[1],x.stop[length(x.stop)],len=grid.res)
longs<-seq(y.start[1],y.stop[length(y.stop)],len=grid.res)

# plot with map overlay
image(z=im.mat,x=lats,y=longs,
      col=heat.colors(100),xlab="Longitude",ylab="Latitude",
      main="EDA aggregate data",asp=1)
map('worldHires',regions=c("Italy","Sardinia","Sicily"),add=TRUE)



