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

#run_altmods<-function(fixdat,n.grid,plot.it=FALSE,year=""){
   fullll<-data.frame(lat= c(fixdat$italy$dat$lat,
                              fixdat$sicily$dat$lat,
                              fixdat$sardinia$dat$lat),
                       long=c(fixdat$italy$dat$long,
                              fixdat$sicily$dat$long,
                              fixdat$sardinia$dat$long),
                       share_100=c(fixdat$italy$dat$share_100,
                                   fixdat$sicily$dat$share_100,
                                   fixdat$sardinia$dat$share_100),
                       altimetry=c(fixdat$italy$dat$altimetry,
                                   fixdat$sicily$dat$altimetry,
                                   fixdat$sardinia$dat$altimetry))
   
   fulldat<-data.frame(km.e=c(fixdat$italy$dat$km.e,
                              fixdat$sicily$dat$km.e,
                              fixdat$sardinia$dat$km.e),
                       km.n=c(fixdat$italy$dat$km.n,
                              fixdat$sicily$dat$km.n,
                              fixdat$sardinia$dat$km.n),
                       share_100=c(fixdat$italy$dat$share_100,
                                   fixdat$sicily$dat$share_100,
                                   fixdat$sardinia$dat$share_100),
                       altimetry=c(fixdat$italy$dat$altimetry,
                                   fixdat$sicily$dat$altimetry,
                                   fixdat$sardinia$dat$altimetry))
   
   zlim<-c(0,12)

   # plot the raw data
   eda_rets<-do_eda(fullll,plot.it=plot.it,zlim=zlim,year=year)

   xlim<-eda_rets$xlim
   ylim<-eda_rets$ylim

   all.list<-list(x=c(fixdat$italy$map$km.e,NA,
                      fixdat$sicily$map$km.e,NA,
                      fixdat$sardinia$map$km.e),
                  y=c(fixdat$italy$map$km.n,NA,
                      fixdat$sicily$map$km.n,NA,
                      fixdat$sardinia$map$km.n))

   ## same boundary as above but in a different format
   all.bnd<-list(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),
                 list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n),
                 list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n))
   
   soap.knots<-make_soap_grid(all.list,c(20,30))
   soap.knots<-pe(soap.knots,-c(2,14,41,48,64,128))
   
   names(fulldat)<-c("x","y","share_100","altimetry")
   

   fulldat<-data.frame(x=fixdat$italy$dat$km.e,
                       y=fixdat$italy$dat$km.n,
                       share_100=fixdat$italy$dat$share_100,
                       altimetry=fixdat$italy$dat$altimetry)

   it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

   soap.knots<-make_soap_grid(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),c(20,30))
   soap.knots<-pe(soap.knots,-c(11,32,47,121))

   b.soap<- gam(share_100~ 
      te(x,y,altimetry,bs=c("sf","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
      te(x,y,altimetry,bs=c("sw","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=list(it)),NULL))
               ,knots=soap.knots,data=fulldat,family=Gamma(link="log"))


#   knot.bnd<-list(x=c(fixdat$italy$map$km.e,NA,fixdat$sicily$map$km.e,NA,fixdat$sardinia$map$km.e),
#                  y=c(fixdat$italy$map$km.n,NA,fixdat$sicily$map$km.n,NA,fixdat$sardinia$map$km.n))
#
#
#   soap.knots<-make_soap_grid(knot.bnd,c(20,30))
#   soap.knots<-pe(soap.knots,-c(2,14,41,48,64,128))
#
#   b.soap<- gam(share_100~ 
#      te(x,y,altimetry,bs=c("sf","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=all.bnd),NULL))+
#      te(x,y,altimetry,bs=c("sw","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=all.bnd),NULL))
#               ,knots=soap.knots,data=fulldat,family=Gamma(link="log"))



   if(plot.it){

      par(mfrow=c(2,2))

      n.grid<-150   

      # average fit
      vis.gam(b.soap,plot.type="contour",n.grid=n.grid,too.far=0.01,type="response",
              main=paste("Soap film smoother (2008)"),asp=1,color="topo",xlim=xlim,ylim=ylim,zlim=zlim,
              xlab="km (e)",ylab="km (n)",cex.main=1.4,cex.lab=1.4,cex.axis=1.3,lwd=0.7)
      lines(it,lwd=2)


      m<-150;n<-150
      xm <- seq(min(it$x),max(it$x),length=m);yn<-seq(min(it$y),max(it$y),length=n)
      xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
      onoff<-inSide(it,xx,yy)
      xx<-xx[onoff];yy<-yy[onoff]
      pred.mat<-matrix(NA,m,n)

      
      # plains
      pred.grid.131<-list(x=xx,y=yy,altimetry=rep(131,length(xx)))
      z.131<-predict(b.soap,newdata=pred.grid.131,type="response")
      pred.mat[onoff]<-z.131
      image(xm,yn,pred.mat,col=topo.colors(100),
                 main=paste("Soap film smoother (2008), altimetry=131"),asp=1,
                 xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
                 cex.lab=1.4,cex.axis=1.3,lwd=0.7)
      contour(xm,yn,pred.mat,levels=seq(min(z.131),max(z.131),by=0.75),add=TRUE)
      lines(it,lwd=2)

      # hills
      pred.grid.431<-list(x=xx,y=yy,altimetry=rep(431,length(xx)))
      z.431<-predict(b.soap,newdata=pred.grid.431,type="response")
      pred.mat[onoff]<-z.431
      image(xm,yn,pred.mat,col=topo.colors(100),
                 main=paste("Soap film smoother (2008), altimetry=431"),asp=1,
                 xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
                 cex.lab=1.4,cex.axis=1.3,lwd=0.7)
      contour(xm,yn,pred.mat,levels=seq(min(z.431),max(z.431),by=0.75),add=TRUE)
      lines(it,lwd=2)

      # plains
      pred.grid.838<-list(x=xx,y=yy,altimetry=rep(838,length(xx)))
      z.838<-predict(b.soap,newdata=pred.grid.838,type="response")
      pred.mat[onoff]<-z.838
      image(xm,yn,pred.mat,col=topo.colors(100),
                 main=paste("Soap film smoother (2008), altimetry=838"),asp=1,
                 xlim=xlim,ylim=ylim,zlim=zlim,xlab="km (e)",ylab="km (n)",cex.main=1.4,
                 cex.lab=1.4,cex.axis=1.3,lwd=0.7)
      contour(xm,yn,pred.mat,levels=seq(min(z.838),max(z.838),by=0.75),add=TRUE)
      lines(it,lwd=2)



   }

   # return the models
#   return(b.soap)

#}
