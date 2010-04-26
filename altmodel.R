# testing the model with altimetry and as.factor(Regions) 

# packages
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

zlim<-c(0,12)

### build a grid   
# use adehabitat to make the grid for us...
grid.res<-100


########################
# Italy

# select only 2008 data...
it2008<-fixdat$italy$dat
it2008<-as.data.frame(pe(it2008,it2008$year==2008))
it2008$x<-it2008$km.e
it2008$y<-it2008$km.n
it2008$km.e<-NULL
it2008$km.n<-NULL


# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

# setup the soap knots
#20x25
soap.knots<-make_soap_grid(it,c(20,25))
soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108))

onoff<-inSide(it,it2008$x,it2008$y)

it2008<-pe(it2008,onoff)

it.soap<- gam(share_100~
   te(x,y,altimetry,bs=c("sf","cr"),k=c(50,10),d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
   te(x,y,altimetry,bs=c("sw","cr"),k=c(50,10),d=c(2,1),xt=list(list(bnd=list(it)),NULL))
            ,knots=soap.knots,data=it2008,family=Gamma(link="log"),method="REML")

save.image(file="altmod.RData")








#par(mfrow=c(2,2))
#
#n.grid<-150   
#
## average fit
#vis.gam(it.soap,plot.type="contour",n.grid=n.grid,too.far=0.01,type="response",
#        main=paste("Soap film smoother (2008)"),asp=1,color="heat",#xlim=xlim,ylim=ylim,zlim=zlim,
#        xlab="km (e)",ylab="km (n)",cex.main=1.4,cex.lab=1.4,cex.axis=1.3,lwd=0.7)
#lines(it,lwd=2)
#
#
#m<-150;n<-150
#xm <- seq(min(it$x),max(it$x),length=m);yn<-seq(min(it$y),max(it$y),length=n)
#xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
#onoff<-inSide(it,xx,yy)
#xx<-xx[onoff];yy<-yy[onoff]
#pred.mat<-matrix(NA,m,n)
#
## do all the predictions here
#pred.grid.131<-list(x=xx,y=yy,altimetry=rep(131,length(xx)))
#z.131<-predict(b.soap,newdata=pred.grid.131,type="response")
#pred.grid.431<-list(x=xx,y=yy,altimetry=rep(431,length(xx)))
#z.431<-predict(b.soap,newdata=pred.grid.431,type="response")
#pred.grid.838<-list(x=xx,y=yy,altimetry=rep(838,length(xx)))
#z.838<-predict(b.soap,newdata=pred.grid.838,type="response")
#
#zlim<-c(0,20)
#
## now the plots
## plains
#pred.mat[onoff]<-z.131
#image(xm,yn,pred.mat,col=topo.colors(1000),
#           main="2008, plain",asp=1,
#           xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
#           cex.lab=1.4,cex.axis=1.3,lwd=0.7)
#contour(xm,yn,pred.mat,levels=seq(zlim[1],zlim[2],by=1),col="red",add=TRUE)
#lines(it,lwd=2)
#
## hills
#pred.mat[onoff]<-z.431
#image(xm,yn,pred.mat,col=topo.colors(1000),
#           main="2008, hill",asp=1,
#           xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
#           cex.lab=1.4,cex.axis=1.3,lwd=0.7)
#contour(xm,yn,pred.mat,levels=seq(zlim[1],zlim[2],by=1),col="red",add=TRUE)
#lines(it,lwd=2)
#
## mountains
#pred.mat[onoff]<-z.838
#image(xm,yn,pred.mat,col=topo.colors(1000),
#           main="2008, mountain",asp=1,
#           xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
#           cex.lab=1.4,cex.axis=1.3,lwd=0.7)
#contour(xm,yn,pred.mat,levels=seq(zlim[1],zlim[2],by=1),col="red",add=TRUE)
#lines(it,lwd=2)
#
#
#
#   }

   # return the models
#   return(b.soap)

#}
