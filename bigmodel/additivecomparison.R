# huge model -- additive version
# this is the big fat version that does Italy, Sardinia and Sicily...

library(maps)
library(mapdata)
library(soap)
library(dillhandy)

# extra scripts
source("fixit.R")

eps<-0
eps<-1e-8

# run fixdata anyway to get the boundaries
full<-read.csv(file="database_complete.csv")
fixdat<-fix_it_data(full)

########################
# Italy
# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)
# Sardinia boundary
sa<-list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n)
# Sicily boundary
sc<-list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n)

#############################
# Italy
it.dat<-list(x=fixdat$italy$dat$km.e,
          y=fixdat$italy$dat$km.n,
          year=fixdat$italy$dat$year,
          share_100=fixdat$italy$dat$share_100+eps)

# basis size
it.bsize<-c(120,6)

it.add<- gam(share_100~s(x,y,k=it.bsize[1])+s(year,bs="cr",k=it.bsize[2]),
#              data=it.dat,family=Tweedie(link=power(0),p=1.7),method="REML")
             data=it.dat,family=Gamma(link="log"),method="REML")
##########################
gc()

#save.image("it.RData")

########################
# Sardinia 
sa.dat<-list(x=fixdat$sardinia$dat$km.e,
          y=fixdat$sardinia$dat$km.n,
          year=fixdat$sardinia$dat$year,
          share_100=fixdat$sardinia$dat$share_100+eps)

sa.ksize<-c(60,6)

sa.add<- gam(share_100~s(x,y,k=sa.ksize[1])+s(year,bs="cr",k=sa.ksize[2]),
#            data=sa.dat,family=Tweedie(link=power(0),p=1.6),method="REML")
           data=sa.dat,family=Gamma(link="log"),method="REML")
##########################
gc()

########################
# Sicily 
sc.dat<-list(x=fixdat$sicily$dat$km.e,
          y=fixdat$sicily$dat$km.n,
          year=fixdat$sicily$dat$year,
          share_100=fixdat$sicily$dat$share_100+eps)

# setup the soap knots
sc.bsize<-c(50,6)
sc.add<- gam(share_100~
   s(x,y,k=sc.bsize[1])+s(year,bs="cr",k=sc.bsize[2]),
#            data=sc.dat,family=Tweedie(link=power(0),p=1.6),method="REML")
            data=sc.dat,family=Gamma(link="log"),method="REML")
##########################
gc()


########################
# now make the image plot

# options
grid.res.x<-100
grid.res.y<-60
years<-as.numeric(levels(as.factor(it.dat$year)))

# setup the prediction grid
xmin<-min(c(it$x,sa$x,sc$x))
ymin<-min(c(it$y,sa$y,sc$y))
xmax<-max(c(it$x,sa$x,sc$x))
ymax<-max(c(it$y,sa$y,sc$y))
xm <- seq(xmin,xmax,length=grid.res.x);yn<-seq(ymin,ymax,length=grid.res.y)
xx <- rep(xm,grid.res.y);yy<-rep(yn,rep(grid.res.x,grid.res.y))
im.mat<-matrix(NA,length(years),grid.res.x*grid.res.y)

# which grid points relate to which places?
it.onoff<-inSide(it,xx,yy)
sa.onoff<-inSide(sa,xx,yy)
sc.onoff<-inSide(sc,xx,yy)

# do the prediction
for (i in 1:length(years)){
   pred.grid<-list(x=xx,y=yy,year=rep(years[i],length(xx)))
   # italy
   im.mat[i,it.onoff]<-predict(it.add,pe(pred.grid,it.onoff),type="response")
   # sardinia
   im.mat[i,sa.onoff]<-predict(sa.add,pe(pred.grid,sa.onoff),type="response")
   # sicily
   im.mat[i,sc.onoff]<-predict(sc.add,pe(pred.grid,sc.onoff),type="response")
}

# limits for the plot   
xlim<-c(xm[1]-25,xm[length(xm)])
ylim<-c(yn[1]-25,yn[length(yn)]+25)
zlim<-c(0,12)

######################
# SAVE
######################
save.image(paste("add-mod-",it.add$family[[1]],".RData",sep=""))


pdf(paste("add-maps-",it.add$family[[1]],".pdf",sep=""),width=9)
par(mfrow=c(2,3),mar=c(4.5,4.5,2,2))

for (i in 1:length(years)){
   # plot the image
   image(z=matrix(im.mat[i,],grid.res.x,grid.res.y),x=xm,y=yn,
         col=heat.colors(100),xlab="km (e)",ylab="km (n)",
         main=years[i],asp=1,cex.main=1.4,
         cex.lab=1.4,cex.axis=1.3,zlim=zlim,xlim=xlim,ylim=ylim)

   # then the contour ontop
   contour(xm,yn,matrix(im.mat[i,],grid.res.x,grid.res.y),
            levels=seq(zlim[1],zlim[2],by=2),col="blue",add=TRUE)

   # then the country borders
   lines(it,lwd=2)
   lines(sa,lwd=2)
   lines(sc,lwd=2)

}
dev.off()

pdf(paste("add-gamcheck-it-",it.add$family[[1]],".pdf",sep=""),width=5)
gam.check(it.add)
dev.off()
pdf(paste("add-gamcheck-sa-",it.add$family[[1]],".pdf",sep=""),width=5)
gam.check(sa.add)
dev.off()
pdf(paste("add-gamcheck-sc-",it.add$family[[1]],".pdf",sep=""),width=5)
gam.check(sc.add)
dev.off()
