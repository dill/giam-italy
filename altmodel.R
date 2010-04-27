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

#save.image(file="altmod.RData")

##############################
## sicily and sardinia models


## sicily
sc2008<-fixdat$sicily$dat
sc2008<-as.data.frame(pe(sc2008,sc2008$year==2008))
sc2008$x<-sc2008$km.e
sc2008$y<-sc2008$km.n
sc2008$km.e<-NULL
sc2008$km.n<-NULL

# boundary
sc<-list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n)

# check everything is inside
onoff<-inSide(sc,sc2008$x,sc2008$y)
sc2008<-pe(sc2008,onoff)

# soap knots
sc.knots<-make_soap_grid(sc,c(10,10))

sc.soap<- gam(share_100~
   te(x,y,altimetry,bs=c("sf","cr"),k=c(20,10),d=c(2,1),xt=list(list(bnd=list(sc)),NULL))+
   te(x,y,altimetry,bs=c("sw","cr"),k=c(20,10),d=c(2,1),xt=list(list(bnd=list(sc)),NULL))
            ,knots=sc.knots,data=sc2008,family=Gamma(link="log"),method="REML")



##############################
## sardinia


sa2008<-fixdat$sardinia$dat
sa2008<-as.data.frame(pe(sa2008,sa2008$year==2008))
sa2008$x<-sa2008$km.e
sa2008$y<-sa2008$km.n
sa2008$km.e<-NULL
sa2008$km.n<-NULL

# boundary
sa<-list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n)

# check everything is inside
onoff<-inSide(sa,sa2008$x,sa2008$y)
sa2008<-pe(sa2008,onoff)

# soap knots
sa.knots<-make_soap_grid(sa,c(10,10))

sa.soap<- gam(share_100~
   te(x,y,altimetry,bs=c("sf","cr"),k=c(20,10),d=c(2,1),xt=list(list(bnd=list(sa)),NULL))+
   te(x,y,altimetry,bs=c("sw","cr"),k=c(20,10),d=c(2,1),xt=list(list(bnd=list(sa)),NULL))
            ,knots=sa.knots,data=sa2008,family=Gamma(link="log"),method="REML")

############################

# STOP. Pediction time.



# prediction grids!
m<-150;n<-150
# find grid max and mins
gmaxx<-max(it$x,sc$x,sa$x)
gmaxy<-max(it$y,sc$y,sa$y)
gminx<-min(it$x,sc$x,sa$x)
gminy<-min(it$y,sc$y,sa$y)
# make the prediciton grid
xm <- seq(gminx,gmaxx,length=m)
yn<-seq(gminy,gmaxy,length=n)
xx <- rep(xm,n)
yy<-rep(yn,rep(m,n))


xlim<-c(xm[1]-25,xm[length(xm)])
ylim<-c(yn[1]-25,yn[length(yn)]+25)
zlim<-c(0,12)


#onoff<-inSide(it,xx,yy)
#xx<-xx[onoff];yy<-yy[onoff]
pred.mat<-matrix(NA,m,n)




par(mfrow=c(2,2))

n.grid<-150   

# average fit
#vis.gam(it.soap,plot.type="contour",n.grid=n.grid,too.far=0.01,type="response",
#        main=paste("Soap film smoother (2008)"),asp=1,color="heat",xlim=xlim,ylim=ylim,zlim=zlim,
#        xlab="km (e)",ylab="km (n)",cex.main=1.4,cex.lab=1.4,cex.axis=1.3,lwd=0.7)
#lines(it,lwd=2)


# make the prediciton grids 
pred.grid.131<-list(x=xx,y=yy,altimetry=rep(131,length(xx)))
pred.grid.431<-list(x=xx,y=yy,altimetry=rep(431,length(xx)))
pred.grid.838<-list(x=xx,y=yy,altimetry=rep(838,length(xx)))

# actually do the prediction!
it.z.131<-predict(it.soap,newdata=pred.grid.131,type="response")
it.z.431<-predict(it.soap,newdata=pred.grid.431,type="response")
it.z.838<-predict(it.soap,newdata=pred.grid.838,type="response")

sa.z.131<-predict(sa.soap,newdata=pred.grid.131,type="response")
sa.z.431<-predict(sa.soap,newdata=pred.grid.431,type="response")
sa.z.838<-predict(sa.soap,newdata=pred.grid.838,type="response")

sc.z.131<-predict(sc.soap,newdata=pred.grid.131,type="response")
sc.z.431<-predict(sc.soap,newdata=pred.grid.431,type="response")
sc.z.838<-predict(sc.soap,newdata=pred.grid.838,type="response")

#####################################
# plotting

## average model!!



# plains
pred.mat[inSide(it,xx,yy)]<-it.z.131
pred.mat[inSide(sc,xx,yy)]<-sc.z.131
pred.mat[inSide(sa,xx,yy)]<-sa.z.131
image(xm,yn,pred.mat,col=heat.colors(1000),
           main="2008, plain",asp=1,
           xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
           cex.lab=1.4,cex.axis=1.3,lwd=0.7)
contour(xm,yn,pred.mat,levels=seq(zlim[1],zlim[2],by=1),col="green",add=TRUE)
lines(it,lwd=2)

# hills
pred.mat[inSide(it,xx,yy)]<-it.z.431
pred.mat[inSide(sc,xx,yy)]<-sc.z.431
pred.mat[inSide(sa,xx,yy)]<-sa.z.431
image(xm,yn,pred.mat,col=heat.colors(1000),
           main="2008, hill",asp=1,
           xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
           cex.lab=1.4,cex.axis=1.3,lwd=0.7)
contour(xm,yn,pred.mat,levels=seq(zlim[1],zlim[2],by=1),col="green",add=TRUE)
lines(it,lwd=2)

# mountains
pred.mat[inSide(it,xx,yy)]<-it.z.838
pred.mat[inSide(sc,xx,yy)]<-sc.z.838
pred.mat[inSide(sa,xx,yy)]<-sa.z.838
image(xm,yn,pred.mat,col=heat.colors(1000),
           main="2008, mountain",asp=1,
           xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
           cex.lab=1.4,cex.axis=1.3,lwd=0.7)
contour(xm,yn,pred.mat,levels=seq(zlim[1],zlim[2],by=1),col="green",add=TRUE)
lines(it,lwd=2)


