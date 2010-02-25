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



# function to do the formatting to the data
fix_it_data<-function(data){

   ##### Mainland
   # make a copy of the data
   it.dat<-data
   # first grab the shape of Italy from the mapdata package
   it.map<-map('worldHires',regions='Italy',plot=FALSE,exact=TRUE,boundary=TRUE)

   # using inSide from soap, get rid of the non-mainland points
   # islands are separated from the mainland by NAs
   ind<-c(1:length(it.map$x))
   ind[is.na(it.map$x)]<-NA
   
   end<-c(which(is.na(ind))-1,length(ind))
   start<-c(1,which(is.na(ind))+1)
   
   for(i in 1:length(start)){
      it.map$x[start[i]:end[i]]<-rev(it.map$x[start[i]:end[i]])
      it.map$y[start[i]:end[i]]<-rev(it.map$y[start[i]:end[i]])
   }
   
   it.map<-list(x=c(it.map$x[!is.na(it.map$x)],it.map$x[1]),
                y=c(it.map$y[!is.na(it.map$x)],it.map$y[1]))

   # simplify the boundary
   it.map<-pe(it.map,c(seq(1,length(it.map$x),100),1))
   
   # find the mainland
   onoff<-inSide(it.map,it.dat$longitude,it.dat$latitude)
   it.dat<-pe(it.dat,onoff)

   # now it.dat contains only the points on the mainland

   # convert to northings and eastings
   # km relative to 11.5,44 (approx centroid)
   ne.km<-latlong2km(it.dat$longitude,it.dat$latitude,11.5,44)

   # add the northings and eastings into the dataframe 
   it.dat$km.e<-ne.km$km.e
   it.dat$km.n<-ne.km$km.n

   # make share_100 work with the log link
   it.dat$share_100<-it.dat$share_100+1e-6

   # also return the boundary in km
   it.map<-latlong2km(it.map$x,it.map$y,11.5,44)

   ### Sicily
   sc.dat<-data
   sc.map<-map('worldHires',regions='Sicily',plot=FALSE,exact=TRUE,boundary=TRUE)
   # simplify the boundary
   sc.map<-pe(sc.map,c(seq(1,length(sc.map$x),50),1))

   ind<-c(1:length(sc.map$x))
   onoff<-inSide(sc.map,sc.dat$longitude,sc.dat$latitude)
   sc.dat<-pe(sc.dat,onoff)
   ne.km<-latlong2km(sc.dat$longitude,sc.dat$latitude,11.5,44)

   # add the northings and eastings into the dataframe 
   sc.dat$km.e<-ne.km$km.e
   sc.dat$km.n<-ne.km$km.n

   # make share_100 work with the log link
   sc.dat$share_100<-sc.dat$share_100+1e-6

   # also return the boundary in km
   sc.map<-latlong2km(sc.map$x,sc.map$y,11.5,44)

   ### Sardinia
   sa.dat<-data
   sa.map<-map('worldHires',regions='Sardinia',plot=FALSE,exact=TRUE,boundary=TRUE)
   sa.map<-pe(sa.map,c(seq(1,length(sa.map$x),50),1))

   ind<-c(1:length(sa.map$x))
   onoff<-inSide(sa.map,sa.dat$longitude,sa.dat$latitude)
   sa.dat<-pe(sa.dat,onoff)

   # add the northings and eastings into the dataframe 
   ne.km<-latlong2km(sa.dat$longitude,sa.dat$latitude,11.5,44)
   sa.dat$km.e<-ne.km$km.e
   sa.dat$km.n<-ne.km$km.n

   # make share_100 work with the log link
   sa.dat$share_100<-sa.dat$share_100+1e-6

   # also return the boundary in km
   sa.map<-latlong2km(sa.map$x,sa.map$y,11.5,44)

   return(list(italy=list(dat=it.dat,map=it.map),
               sicily=list(dat=sc.dat,map=sc.map),
               sardinia=list(dat=sa.dat,map=sa.map)))
} 




run_mods<-function(fixdat,n.grid,plot.it=FALSE,year=""){
   fullll<-data.frame(lat= c(fixdat$italy$dat$latitude,
                              fixdat$sicily$dat$latitude,
                              fixdat$sardinia$dat$latitude),
                       long=c(fixdat$italy$dat$longitude,
                              fixdat$sicily$dat$longitude,
                              fixdat$sardinia$dat$longitude),
                       share_100=c(fixdat$italy$dat$share_100,
                                   fixdat$sicily$dat$share_100,
                                   fixdat$sardinia$dat$share_100))
   
   fulldat<-data.frame(km.e=c(fixdat$italy$dat$km.e,
                              fixdat$sicily$dat$km.e,
                              fixdat$sardinia$dat$km.e),
                       km.n=c(fixdat$italy$dat$km.n,
                              fixdat$sicily$dat$km.n,
                              fixdat$sardinia$dat$km.n),
                       share_100=c(fixdat$italy$dat$share_100,
                                   fixdat$sicily$dat$share_100,
                                   fixdat$sardinia$dat$share_100))
   
   
   # fit the thin plate spline models 
   
   # first the full model (italy+sardinia+sicily)
   full.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fulldat)
   
## individual models for Italy, Sicily, Sardinia
#   # italy
#   it.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fixdat$italy$dat)
#   # sicily
#   sc.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fixdat$sicily$dat)
#   # sardinia
#   sa.b<-gam(share_100~s(km.e,km.n,k=100),family=Gamma(link="log"),data=fixdat$sardinia$dat)
   
   # time for some plots
   #par(mfrow=c(2,3))
   
   zlim<-c(0,12)

   # plot the raw data
   eda_rets<-do_eda(fullll,plot.it=plot.it,zlim=zlim,year=year)

   xlim<-eda_rets$xlim
   ylim<-eda_rets$ylim

   if(plot.it){
      # vis.gam plots!
      vis.gam(full.b,plot.type="contour",n.grid=n.grid,too.far=0.01,type="response",
              main=paste("TPRS",year),asp=1,color="topo",xlim=xlim,ylim=ylim,zlim=zlim,
              xlab="km (e)",ylab="km (n)",cex.main=1.4,cex.lab=1.4,cex.axis=1.3,lwd=0.7)
      lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)
      lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)
      lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)
      
## individual plots for Italy, Sicily, Sardinia
#      vis.gam(it.b,plot.type="contour",n.grid=n.grid,too.far=0.01,type="response",
#              main="TPRS Italy",asp=1,color="topo",xlim=xlim,ylim=ylim,zlim=zlim,
#              xlab="km (e)",ylab="km (n)")
#      lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)
#      
#      vis.gam(sc.b,plot.type="contour",n.grid=n.grid,too.far=0.1,type="response",
#              main="TPRS Sicily",asp=1,color="topo",xlim=xlim,ylim=ylim,zlim=zlim,
#              xlab="km (e)",ylab="km (n)")
#      lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)
#      
#      vis.gam(sa.b,plot.type="contour",n.grid=n.grid,too.far=0.1,type="response",
#              main="TPRS Sardinia",asp=1,color="topo",xlim=xlim,ylim=ylim,zlim=zlim,
#              xlab="km (e)",ylab="km (n)")
#      lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)
   }
   
   # now using soap
   # first create a grid
   
   # for the sake of simpicity switch from km.e and km.n to x and y for soap
   
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
   
   names(fulldat)<-c("x","y","share_100")
   
   b.soap<-gam(share_100~s(x,y,k=50,bs="so",xt=list(bnd=all.bnd)),
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
   return(list(tprs.mod=full.b,soap.mod=b.soap))

}
