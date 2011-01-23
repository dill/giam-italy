# smooth the ISEA data - Italy only

# load some relevant libraries
library(maps)
library(mapdata)
library(soap)
library(dillhandy)

# script to put the data in the right format
source("fixit.R")

# run fixdata anyway to get the boundaries
full<-read.csv(file="database_index.csv")
fixdat<-fix_it_data(full)

# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

########################
# Italy

# get the data
it.dat<-list(x=fixdat$italy$dat$km.e,
          y=fixdat$italy$dat$km.n,
          year=fixdat$italy$dat$year,
          index=fixdat$italy$dat$index)

# basis size
it.bsize<-c(20,6)
# setup the soap knots
soap.knots.it<-make_soap_grid(it,c(7,7))

# set the Tweedie parameter
tweediepar<-1.7

it.soap<- gam(index~
   te(x,y,year,bs=c("sf","cr"),k=it.bsize,d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=it.bsize,d=c(2,1),xt=list(list(bnd=list(it)),NULL))
         ,knots=soap.knots.it,data=it.dat,family=Tweedie(link=power(0),p=tweediepar),method="REML")
# comment the line above and uncomment the line below for Gamma errors
#         ,knots=soap.knots.it,data=it.dat,family=Gamma(link=power(0)),method="REML")
##########################
gc()

########################
# now make the image plot

# spatial and temporal resolution of the plot
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

# which grid points are inside Italy 
it.onoff<-inSide(it,xx,yy)

# do the prediction
for (i in 1:length(years)){
   pred.grid<-list(x=xx,y=yy,year=rep(years[i],length(xx)))
   im.mat[i,it.onoff]<-predict(it.soap,pe(pred.grid,it.onoff),type="response")
}

# limits for the plot   
xlim<-c(xm[1]-25,xm[length(xm)]+25)
ylim<-c(yn[1]-25,yn[length(yn)]+25)
zlim<-c(9,93)

######################
# SAVE
######################
save.image(paste("indexmod-",it.soap$family[[1]],".RData",sep=""))


######################
# PLOTTING 
######################

#pdf(paste("index-maps-",it.soap$family[[1]],"-",tweediepar,".pdf",sep=""),width=9)
postscript(paste("index-maps-",it.soap$family[[1]],".ps",sep=""),width=9)
par(mfrow=c(2,3),mar=c(4.5,4.5,2,2))

for (i in 1:length(years)){
   # plot the image
   image(z=matrix(im.mat[i,],grid.res.x,grid.res.y),x=xm,y=yn,
         col=heat.colors(100),xlab="km (e)",ylab="km (n)",
         main=years[i],asp=1,cex.main=1.4,
         cex.lab=1.4,cex.axis=1.3,zlim=zlim,xlim=xlim,ylim=ylim)

   # then the contour ontop
   contour(xm,yn,matrix(im.mat[i,],grid.res.x,grid.res.y),
            levels=seq(zlim[1],zlim[2],by=10),col="blue",add=TRUE)

   # Italian border
   lines(it,lwd=2)

}
dev.off()

