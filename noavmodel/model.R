# read in av data and run the model
# this is the big fat version that does Italy, Sardinia and Sicily...

library(maps)
library(mapdata)
library(soap)
library(dillhandy)

# extra scripts
source("fixit.R")

# run fixdata anyway to get the boundaries
full<-read.csv(file="database_complete.csv")
fixdat<-fix_it_data(full)

########################
# Italy
# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)
# data
it.dat<-list(x=fixdat$italy$dat$km.e,
             y=fixdat$italy$dat$km.n,
             year=fixdat$italy$dat$year,
             share_100=fixdat$italy$dat$share_100)#+1e-15)

# setup the soap knots
soap.knots<-make_soap_grid(it,c(15,15))
#soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108)) #20 x 25
soap.knots<-pe(soap.knots,-c(1,46)) #15 x15

# basis size
it.bsize<-c(20,12)

it.soap<- gam(share_100~
   te(x,y,year,bs=c("sf","cr"),k=it.bsize,d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=it.bsize,d=c(2,1),xt=list(list(bnd=list(it)),NULL))
#            ,knots=soap.knots,data=it.dat,family=Tweedie(link=power(1),p=1.6),method="REML")
            ,knots=soap.knots,data=it.dat,family=Tweedie(link=power(0),p=1.6),method="REML")
#            ,knots=soap.knots,data=it.dat,family=Gamma(link="log"),method="REML")
##########################
gc()

save.image("it.RData")

#########################
## Sardinia 
#
## Sardinia boundary
#sa<-list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n)
## data
#sa.dat<-list(x=fixdat$sardinia$dat$km.e,
#             y=fixdat$sardinia$dat$km.n,
#             year=fixdat$sardinia$dat$year,
#             share_100=fixdat$sardinia$dat$share_100)
#
## setup the soap knots
##20x25
#soap.knots<-make_soap_grid(sa,c(10,10))
#soap.knots<-pe(soap.knots,-c(47))
#
#sa.ksize<-c(10,4)
#
#sa.soap<- gam(share_100~
#   te(x,y,year,bs=c("sf","cr"),k=sc.ksize,d=c(2,1),xt=list(list(bnd=list(sa)),NULL))+
#   te(x,y,year,bs=c("sw","cr"),k=sc.ksize,d=c(2,1),xt=list(list(bnd=list(sa)),NULL))
#            ,knots=soap.knots,data=av.dat.sa,family=Gamma(link="log"),method="REML")
##sa.soap<- gam(share_100~
##   te(x,y,year,bs=c("sf","cr"),k=sa.ksize,d=c(2,1),xt=list(list(bnd=list(sa)),NULL))+
##   te(x,y,year,bs=c("sw","cr"),k=sa.ksize,d=c(2,1),xt=list(list(bnd=list(sa)),NULL))
##            ,knots=soap.knots,data=av.dat.sa,family=Tweedie(link=power(0),p=1.5),method="REML")
###########################
#gc()
#
#save.image("sa.RData")
#
#
#########################
## Sicily 
#
## Sicily boundary
#sc<-list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n)
## data
#sc.dat<-list(x=fixdat$sicily$dat$km.e,
#             y=fixdat$sicily$dat$km.n,
#             year=fixdat$sicily$dat$year,
#             share_100=fixdat$sicily$dat$share_100)
#
## setup the soap knots
#soap.knots<-make_soap_grid(sc,c(10,10))
##soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108))
#
#sc.bsize<-c(10,4)
#sc.soap<- gam(share_100~
#   te(x,y,year,bs=c("sf","cr"),k=c(20,4),d=c(2,1),xt=list(list(bnd=list(sc)),NULL))+
#   te(x,y,year,bs=c("sw","cr"),k=c(20,4),d=c(2,1),xt=list(list(bnd=list(sc)),NULL))
#            ,knots=soap.knots,data=av.dat.sc,family=Gamma(link="log"),method="REML")
##sc.soap<- gam(share_100~
##   te(x,y,year,bs=c("sf","cr"),k=sc.bsize,d=c(2,1),xt=list(list(bnd=list(sc)),NULL))+
##   te(x,y,year,bs=c("sw","cr"),k=sc.bsize,d=c(2,1),xt=list(list(bnd=list(sc)),NULL))
##            ,knots=soap.knots,data=av.dat.sc,family=Tweedie(link=power(0),p=1.5),method="REML")
###########################
#gc()
#
#save.image("sc.RData")
#
#
#pdf(file="maps.pdf",width=9)
########################
# now make the image plot

# options
grid.res.x<-100
grid.res.y<-60
years<-as.numeric(levels(as.factor(it.dat$year)))

# setup the prediction grid
xmin<-min(it$x)
ymin<-min(it$y)
xmax<-max(it$x)
ymax<-max(it$y)
xm <- seq(xmin,xmax,length=grid.res.x);yn<-seq(ymin,ymax,length=grid.res.y)
xx <- rep(xm,grid.res.y);yy<-rep(yn,rep(grid.res.x,grid.res.y))

im.mat<-matrix(NA,length(years),grid.res.x*grid.res.y)


for (i in 1:length(years)){
   pred.grid<-list(x=xx,y=yy,year=rep(years[i],length(xx)))
   # italy
   im.mat[i,]<-predict(it.soap,pred.grid,type="response")
}

   
   xlim<-c(xm[1]-25,xm[length(xm)])
   ylim<-c(yn[1]-25,yn[length(yn)]+25)
   zlim<-c(0,12)


par(mfrow=c(2,3),mar=c(4.5,4.5,2,2))


for (i in 1:length(years)){
   image(z=matrix(im.mat[i,],grid.res.x,grid.res.y),x=xm,y=yn,
         col=heat.colors(100),xlab="km (e)",ylab="km (n)",
         main=years[i],asp=1,cex.main=1.4,
         cex.lab=1.4,cex.axis=1.3,zlim=zlim,xlim=xlim,ylim=ylim)

   contour(xm,yn,matrix(im.mat[i,],grid.res.x,grid.res.y),
            levels=seq(zlim[1],zlim[2],by=1),col="blue",add=TRUE)
   lines(it,lwd=2)

}

x11()
gam.check(it.soap)

#dev.off()
