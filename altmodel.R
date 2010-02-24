# using the new database now...

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

run_altmods<-function(fixdat,n.grid,plot.it=FALSE,year=""){
   fullll<-data.frame(lat= c(fixdat$italy$dat$latitude,
                              fixdat$sicily$dat$latitude,
                              fixdat$sardinia$dat$latitude),
                       long=c(fixdat$italy$dat$longitude,
                              fixdat$sicily$dat$longitude,
                              fixdat$sardinia$dat$longitude),
                       share_100=c(fixdat$italy$dat$share_100,
                                   fixdat$sicily$dat$share_100,
                                   fixdat$sardinia$dat$share_100),
                       altimetry=c(fixdat$italy$dat$altimetry,
                                   fixdat$sicily$dat$altimetry,
                                   fixdat$sardinia$dat$altimetry),
                       Regions=c(fixdat$italy$dat$Regions,
                                   fixdat$sicily$dat$Regions,
                                   fixdat$sardinia$dat$Regions))
   
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
                                   fixdat$sardinia$dat$altimetry),
                       Regions=c(fixdat$italy$dat$Regions,
                                   fixdat$sicily$dat$Regions,
                                   fixdat$sardinia$dat$Regions))
   
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
   
   names(fulldat)<-c("x","y","share_100","altimetry","Regions")
   
#   b.soap<-gam(share_100~te(x,y,altimetry,k=c(50,10),d=c(2,1),bs=c("so","tp"),xt=list(bnd=all.bnd))+
#                         as.factor(Regions),
#                family=Gamma(link="log"),data=fulldat,knots=soap.knots)
#
#
#
#   b.soap<-gam(share_100~te(x,y,altimetry,d=c(2,1),k=c(25,4),bs=c("sf","cr"),xt=list(list(bnd=all.bnd),NULL))+
#                         te(x,y,altimetry,d=c(2,1),k=c(25,4),bs=c("sw","cr"),xt=list(list(bnd=all.bnd),NULL))+
#                         as.factor(Regions),
#                family=Gamma(link="log"),data=fulldat,knots=soap.knots)
#


   fulldat<-data.frame(x=fixdat$italy$dat$km.e,
                       y=fixdat$italy$dat$km.n,
                       share_100=fixdat$italy$dat$share_100,
                       altimetry=fixdat$italy$dat$altimetry,
                       Regions=fixdat$italy$dat$Regions)

   it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)



   soap.knots<-make_soap_grid(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),c(20,30))
#   soap.knots<-pe(soap.knots,-c(2,14,41,48,64,128))

   b.soap<-gam(share_100~te(x,y,altimetry,d=c(2,1),k=c(25,4),bs=c("sf","cr"),xt=list(list(bnd=it),NULL))+
                         te(x,y,altimetry,d=c(2,1),k=c(25,4),bs=c("sw","cr"),xt=list(list(bnd=it),NULL))+
                         as.factor(Regions),
                family=Gamma(link="log"),data=fulldat,knots=soap.knots)

   if(plot.it){
      vis.gam(b.soap,plot.type="contour",n.grid=n.grid,too.far=0.01,type="response",
              main=paste("Soap film smoother",year),asp=1,color="topo",xlim=xlim,ylim=ylim,zlim=zlim,
              xlab="km (e)",ylab="km (n)",cex.main=1.4,cex.lab=1.4,cex.axis=1.3,lwd=0.7)
      lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)
      lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)
      lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)
   }

   # return the models
   return(b.soap)

}
