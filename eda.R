# EDA for the Italian data

do_eda<-function(fullll,plot.it=FALSE,zlim,year,dat.ret=FALSE){
   # want to create a matrix to use with image
   
   # first find the minimum distance between points in both
   # directions
   
   # use adehabitat to make the grid for us...
   grid.res<-150
   
   # convert latlong to eastings and northings
   ne.km<-latlong2km(fullll$long,fullll$lat,11.5,44)
   names(ne.km)<-c("x","y")
   
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
   
   # image matrix
   im.mat<-matrix(NA,grid.res,grid.res)
   
   #i+.mat<-matrix(NA,grid.res,grid.res)
   


#   for(this.year in 

   # put the observations into the grid
   for(i in 1:length(x.start)){
      for(j in 1:length(y.start)){
   
         ind<-ne.km$y>=y.start[j] & ne.km$y<y.stop[j] &
                    ne.km$x>=x.start[i] & ne.km$x<x.stop[i]
   
         # take the mean of the standardised proportion of foreign
         # population, ignoring NAs
         sq<-mean(fullll$share_100[ind],na.rm=T)
         yr<-mean(fullll$year[ind],na.rm=T)
   
         im.mat[i,j]<-sq
         years<-c(years,yr)   
      }
   }

   # need to speed this up...
   #y.ind<-floor((ne.km$y-y.start[1])/diff(y.start)[1])
   #x.ind<-floor((ne.km$x-x.start[1])/diff(x.start)[1])

   #for(i in 1:length(x.start)){
   #   for(j in 1:length(y.start)){
   #
   #      ind<-y.ind[j]+grid.res*x.ind[i]
   #      
   #      # take the mean of the standardised proportion of foreign
   #      # population, ignoring NAs
   #      sq<-mean(fullll$share_100[ind],na.rm=T)
   #      yr<-mean(fullll$year[ind],na.rm=T)
   #
   #      im.mat[i,j]<-sq
   #
   #   }
   #}

   
   im.copy<-im.mat
   im.copy[is.nan(im.copy)]<-0
   seqlen<-sum(rowSums(im.copy)>=0)
   
   x.start<-x.start[rowSums(im.copy)>=0]
   #y.start<-y.stop[rowSums(im.copy)>0]
   x.stop<-x.start[rowSums(im.copy)>=0]
   #y.stop<-y.stop[rowSums(im.copy)>0]
   im.copy<-im.copy[rowSums(im.copy)>=0,]

   
   #im.copy[im.copy==0]<-NA


   
   # create the grid sequences
   xs<-seq(x.start[1],x.stop[length(x.stop)],len=seqlen)
   ys<-seq(y.start[1],y.stop[length(y.stop)],len=grid.res)

   # xlim and ylim   
   xlim=c(xs[1]-25,xs[length(xs)]+25)
   ylim=c(ys[1]-25,ys[length(ys)]+25)
   
   if(plot.it){
      # plot with map overlay
      image(z=im.copy,x=xs,y=ys,
            col=topo.colors(100),xlab="km (e)",ylab="km (n)",
            main=paste("Raw data",year),asp=1,zlim=zlim,cex.main=1.4,
            cex.lab=1.4,cex.axis=1.3,xlim=xlim,ylim=ylim)
      lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n)
      lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n)
      lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n)
   }

   if(dat.ret){

      dat<-data.frame(y=rep(ys,length(xs)),
                      x=rep(xs,rep(length(ys),length(xs))),
                      share_100=as.vector(im.copy),
                      year=years)

      ins.it<-inSide(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),dat$x,dat$y)
      #ins.sc<-inSide(list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n),dat$x,dat$y)
      #ins.sa<-inSide(list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n),dat$x,dat$y)

      ins<-ins.it #& ins.sc & ins.sa

      dat<-pe(dat,ins)


   }else{
      dat<-NULL
   }


   return(list(xlim=xlim,ylim=ylim,dat=dat))
   
} 
