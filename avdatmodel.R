# read in av data and run the model
# this is the big fat version that does Italy, Sardinia and Sicily...

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

# run fixdata anyway to get the boundaries
full<-read.csv(file="database/database_complete.csv")
fixdat<-fix_it_data(full)

# load averaged data
av.dat<-read.csv(file="av.full.dat")

av.dat$year<-as.numeric(av.dat$year)
#av.dat$share_100<-av.dat$share_100+1e-15
av.dat$share_100<-av.dat$share_100+1e-5


########################
# Italy

# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

# setup the soap knots
#20x25
soap.knots<-make_soap_grid(it,c(20,25))
soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108))

onoff<-inSide(it,av.dat$x,av.dat$y)

av.dat.it<-pe(av.dat,onoff)

it.soap<- gam(share_100~
   te(x,y,year,bs=c("sf","cr"),k=c(50,20),d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=c(50,20),d=c(2,1),xt=list(list(bnd=list(it)),NULL))
            ,knots=soap.knots,data=av.dat.it,family=Gamma(link="log"),method="REML")
##########################



########################
# Sardinia 

# Sardinia boundary
sa<-list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n)

# setup the soap knots
#20x25
soap.knots<-make_soap_grid(sa,c(10,10))
soap.knots<-pe(soap.knots,-c(47))

onoff<-inSide(sa,av.dat$x,av.dat$y)

av.dat.sa<-pe(av.dat,onoff)

sa.soap<- gam(share_100~
   te(x,y,year,bs=c("sf","cr"),k=c(20,4),d=c(2,1),xt=list(list(bnd=list(sa)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=c(20,4),d=c(2,1),xt=list(list(bnd=list(sa)),NULL))
            ,knots=soap.knots,data=av.dat.sa,family=Gamma(link="log"),method="REML")
##########################



########################
# Sicily 

# Sicily boundary
sc<-list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n)

# setup the soap knots
#20x25
soap.knots<-make_soap_grid(sc,c(10,10))
#soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108))

onoff<-inSide(sc,av.dat$x,av.dat$y)

av.dat.sc<-pe(av.dat,onoff)

sc.soap<- gam(share_100~
   te(x,y,year,bs=c("sf","cr"),k=c(20,4),d=c(2,1),xt=list(list(bnd=list(sc)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=c(20,4),d=c(2,1),xt=list(list(bnd=list(sc)),NULL))
            ,knots=soap.knots,data=av.dat.sc,family=Gamma(link="log"),method="REML")
##########################



########################
# now make the image plot

# options
grid.res<-100
years<-as.numeric(levels(as.factor(av.dat$year)))

# setup the prediction grid
itmat<-matrix(c(av.dat$x,av.dat$y),length(av.dat$x),2)
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

xm<-(x.start+x.stop)/2
yn<-(y.start+y.stop)/2
xx <- rep(xm,grid.res)
yy<-rep(yn,rep(grid.res,grid.res))

pred.grid<-list(x=xx,y=yy,year=rep(2003,length(xx)))

im.mat<-matrix(NA,grid.res,grid.res)

par(mfrow=c(2,3),mar=c(4.5,4.5,2,2))

for (year in years){

   pred.grid<-list(x=xx,y=yy,year=rep(year,length(xx)))

   im.it<-im.mat

   # sardinia
   z<-predict(sa.soap,pred.grid,type="response")
   im.it[inSide(sa,xx,yy)]<-z[!is.na(z)]

   # sicily
   z<-predict(sc.soap,pred.grid,type="response")
   im.it[inSide(sc,xx,yy)]<-z[!is.na(z)]

   # italy
   z<-predict(it.soap,pred.grid,type="response")
   im.it[inSide(it,xx,yy)]<-z[!is.na(z)]


   im.it<-im.it[1:(grid.res-sum(x.start>620)),]
   xs<-xm[1:(grid.res-sum(x.start>620))]
   ys<-yn
   
   xlim<-c(xs[1]-25,xs[length(xs)])
   ylim<-c(ys[1]-25,ys[length(ys)]+25)
   zlim<-c(0,12)

   image(z=im.it,x=xs,y=ys,
         col=heat.colors(100),xlab="km (e)",ylab="km (n)",
         main=year,asp=1,cex.main=1.4,
         cex.lab=1.4,cex.axis=1.3,xlim=xlim,ylim=ylim,zlim=zlim)

   contour(xs,ys,im.it,levels=seq(zlim[1],zlim[2],by=1),col="blue",add=TRUE)

   lines(it,lwd=2)
   lines(sa,lwd=2)
   lines(sc,lwd=2)

}

