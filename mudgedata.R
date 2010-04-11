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

fullll<-data.frame(lat=c(fixdat$italy$dat$lat),
                   long=c(fixdat$italy$dat$long),
                   share_100=c(fixdat$italy$dat$share_100),
                   altimetry=c(fixdat$italy$dat$altimetry),
                   year=c(fixdat$italy$dat$year))

plot.it<-TRUE
zlim<-c(0,12)
year<-""
par(mfrow=c(2,3))

# need to create a new data frame which has the same data but averaged
# over a lower resolution

### build a grid   
# use adehabitat to make the grid for us...
grid.res<-150

# convert latlong to eastings and northings
ne.km<-latlong2km(fullll$long,fullll$lat,11.5,44)
names(ne.km)<-c("x","y")

fullll$x<-ne.km$x
fullll$y<-ne.km$y

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
av.dat<-list(x=c(),y=c(),share_100=c(),year=c(),alt=c())


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
         av.dat$y<-c(av.dat$y,(y.start[i]+y.stop[i])/2)
         
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
   
#   x.start<-x.start[rowSums(im.copy)>0]
#   #y.start<-y.stop[rowSums(im.copy)>0]
#   x.stop<-x.start[rowSums(im.copy)>0]
#   #y.stop<-y.stop[rowSums(im.copy)>0]
#   im.copy<-im.copy[rowSums(im.copy)>0,]

   im.copy[im.copy==0]<-NA
   
#   # create the grid sequences
#   xs<-seq(x.start[1],x.stop[length(x.stop)],len=seqlen)
#   ys<-seq(y.start[1],y.stop[length(y.stop)],len=grid.res)
#
#   # xlim and ylim   
#   xlim=c(xs[1]-25,xs[length(xs)]+25)
#   ylim=c(ys[1]-25,ys[length(ys)]+25)
   
   if(plot.it){
      # plot with map overlay
      image(z=im.copy,#x=xs,y=ys,
            col=topo.colors(100),xlab="km (e)",ylab="km (n)",
            main=paste("Raw data",year),asp=1,zlim=zlim,cex.main=1.4,
            cex.lab=1.4,cex.axis=1.3)#,xlim=xlim,ylim=ylim)
      lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)
      lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)
      lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)
   }

}


#   return(list(xlim=xlim,ylim=ylim))
#   
#} 
## testing the model with altimetry and as.factor(Regions) 
#
## packages
#library(maps)
#library(mapdata)
#library(soap)
#library(maps)
#library(adehabitat)
#
## extra scripts
#source("pe.R")
#source("latlong2km.R")
#source("makesoapgrid.R")
#source("eda.R")
#
##run_altmods<-function(fixdat,n.grid,plot.it=FALSE,year=""){
#   fullll<-data.frame(lat= c(fixdat$italy$dat$lat,
#                              fixdat$sicily$dat$lat,
#                              fixdat$sardinia$dat$lat),
#                       long=c(fixdat$italy$dat$long,
#                              fixdat$sicily$dat$long,
#                              fixdat$sardinia$dat$long),
#                       share_100=c(fixdat$italy$dat$share_100,
#                                   fixdat$sicily$dat$share_100,
#                                   fixdat$sardinia$dat$share_100),
#                       altimetry=c(fixdat$italy$dat$altimetry,
#                                   fixdat$sicily$dat$altimetry,
#                                   fixdat$sardinia$dat$altimetry),
#                       year=c(fixdat$italy$dat$year,
#                                   fixdat$sicily$dat$year,
#                                   fixdat$sardinia$dat$year))
#   
#   fulldat<-data.frame(km.e=c(fixdat$italy$dat$km.e,
#                              fixdat$sicily$dat$km.e,
#                              fixdat$sardinia$dat$km.e),
#                       km.n=c(fixdat$italy$dat$km.n,
#                              fixdat$sicily$dat$km.n,
#                              fixdat$sardinia$dat$km.n),
#                       share_100=c(fixdat$italy$dat$share_100,
#                                   fixdat$sicily$dat$share_100,
#                                   fixdat$sardinia$dat$share_100),
#                       altimetry=c(fixdat$italy$dat$altimetry,
#                                   fixdat$sicily$dat$altimetry,
#                                   fixdat$sardinia$dat$altimetry),
#                       year=c(fixdat$italy$dat$year,
#                                   fixdat$sicily$dat$year,
#                                   fixdat$sardinia$dat$year))
#   
#   zlim<-c(0,12)
#
#   # plot the raw data
#   eda_rets<-do_eda(fullll,plot.it=plot.it,zlim=zlim,year="")
#
#   xlim<-eda_rets$xlim
#   ylim<-eda_rets$ylim
#
#   all.list<-list(x=c(fixdat$italy$map$km.e,NA,
#                      fixdat$sicily$map$km.e,NA,
#                      fixdat$sardinia$map$km.e),
#                  y=c(fixdat$italy$map$km.n,NA,
#                      fixdat$sicily$map$km.n,NA,
#                      fixdat$sardinia$map$km.n))
#
#   ## same boundary as above but in a different format
#   all.bnd<-list(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),
#                 list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n),
#                 list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n))
#   
#   soap.knots<-make_soap_grid(all.list,c(20,30))
#   soap.knots<-pe(soap.knots,-c(2,14,41,48,64,128))
#   
#   names(fulldat)<-c("x","y","share_100","year")
#   
#
#   fulldat<-data.frame(x=fixdat$italy$dat$km.e,
#                       y=fixdat$italy$dat$km.n,
#                       share_100=fixdat$italy$dat$share_100,
#                       altimetry=fixdat$italy$dat$altimetry)
#
#   it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)
#
#   soap.knots<-make_soap_grid(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),c(20,30))
#   soap.knots<-pe(soap.knots,-c(11,32,47,121))
#
#   b.soap<- gam(share_100~ 
#      te(x,y,year,bs=c("sf","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
#      te(x,y,year,bs=c("sw","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=list(it)),NULL))
#               ,knots=soap.knots,data=fulldat,family=Gamma(link="log"))
#
#
#   if(plot.it){
#
#      par(mfrow=c(2,2))
#
#      n.grid<-150   
#
#      # average fit
##      vis.gam(b.soap,plot.type="contour",n.grid=n.grid,too.far=0.01,type="response",
##              main=paste("Soap film smoother (2008)"),asp=1,color="topo",xlim=xlim,ylim=ylim,zlim=zlim,
##              xlab="km (e)",ylab="km (n)",cex.main=1.4,cex.lab=1.4,cex.axis=1.3,lwd=0.7)
##      lines(it,lwd=2)
#
#
#      m<-150;n<-150
#      xm <- seq(min(it$x),max(it$x),length=m);yn<-seq(min(it$y),max(it$y),length=n)
#      xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
#      onoff<-inSide(it,xx,yy)
#      xx<-xx[onoff];yy<-yy[onoff]
#      pred.mat<-matrix(NA,m,n)
#
#      zlim<-c(0,20)
#
#      years<-c(2003,2004,2005,2006,2007,2008)
#      year.pred<-list()
#      i<-1
#
#      for(year in years){
#
#         # do all the predictions here
#         pred.grid<-list(x=xx,y=yy,altimetry=rep(131,length(xx)))
#         year.pred[[i]]<-predict(b.soap,newdata=pred.grid,type="response")
#
#         # now the plots
#         pred.mat[onoff]<-year.pred[[i]]
#         image(xm,yn,pred.mat,col=topo.colors(1000),
#                    main=year,asp=1,
#                    xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
#                    cex.lab=1.4,cex.axis=1.3,lwd=0.7)
#         contour(xm,yn,pred.mat,levels=seq(zlim[1],zlim[2],by=1),col="red",add=TRUE)
#         lines(it,lwd=2)
#   
#         i<-i+1
#
#      }
#
#
#   }
#
#   # return the models
##   return(b.soap)
#
##}
