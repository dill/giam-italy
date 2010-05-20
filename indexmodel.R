# index model
# index constructed by Luca

source("models.R")

# run fixdat
inddat<-read.csv(file="database/database_index.csv")
inddat<-fix_it_data(inddat)

# might need to do something like this...
#av.dat$share_100<-av.dat$share_100+1e-15


########################
# Italy

# Italy boundary
it<-list(x=inddat$italy$map$km.e,y=inddat$italy$map$km.n)

# setup the soap knots
soap.knots<-make_soap_grid(it,c(20,15))
#soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108))
soap.knots<-pe(soap.knots,-c(5))

# fiddly
it.inddat<-inddat$italy$dat
it.inddat$x<-it.inddat$km.e
it.inddat$y<-it.inddat$km.n
it.inddat$km.e<-NULL
it.inddat$km.n<-NULL

onoff<-inSide(it,it.inddat$x,it.inddat$y)

it.inddat<-pe(it.inddat,onoff)

k.it<-c(19,6)

it.soap<- gam(index~
   te(x,y,year,bs=c("sf","cr"),k=k.it,d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=k.it,d=c(2,1),xt=list(list(bnd=list(it)),NULL))
            ,knots=soap.knots,data=it.inddat,family=Gamma(link="log"),method="REML")
##########################



#########################
## Sardinia 
#
## Sardinia boundary
#sa<-list(x=inddat$sardinia$map$km.e,y=inddat$sardinia$map$km.n)
#
## setup the soap knots
#soap.knots<-make_soap_grid(sa,c(3,4))
##soap.knots<-pe(soap.knots,-c(47))
#
## fiddly
#sa.inddat<-inddat$sardinia$dat
#sa.inddat$x<-sa.inddat$km.e
#sa.inddat$y<-sa.inddat$km.n
#sa.inddat$km.e<-NULL
#sa.inddat$km.n<-NULL
#
#onoff<-inSide(sa,sa.inddat$x,sa.inddat$y)
#
#sa.inddat<-pe(sa.inddat,onoff)
#
#k.sa<-c(4,4)
#
#sa.soap<- gam(index~
#   te(x,y,year,bs=c("sf","cr"),k=k.sa,d=c(2,1),xt=list(list(bnd=list(sa)),NULL))+
#   te(x,y,year,bs=c("sw","cr"),k=k.sa,d=c(2,1),xt=list(list(bnd=list(sa)),NULL))
#            ,knots=soap.knots,data=sa.inddat,family=Gamma(link="log"),method="REML")
###########################

########################
# Sicily 

# Sicily boundary
sc<-list(x=inddat$sicily$map$km.e,y=inddat$sicily$map$km.n)

# setup the soap knots
soap.knots<-make_soap_grid(sc,c(4,4))

# fiddly
sc.inddat<-inddat$sicily$dat
sc.inddat$x<-sc.inddat$km.e
sc.inddat$y<-sc.inddat$km.n
sc.inddat$km.e<-NULL
sc.inddat$km.n<-NULL

onoff<-inSide(sc,sc.inddat$x,sc.inddat$y)

sc.inddat<-pe(sc.inddat,onoff)


sc.inddat$index<-sc.inddat$index+1e-10

k.sc<-c(4,4)

sc.soap<- gam(index~
   te(x,y,year,bs=c("sf","cr"),k=k.sc,d=c(2,1),xt=list(list(bnd=list(sc)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=k.sc,d=c(2,1),xt=list(list(bnd=list(sc)),NULL))
            ,knots=soap.knots,data=sc.inddat,family=Gamma(link="log"),method="REML")
##########################


########################
# now make the image plot

# options
grid.res<-100
years<-as.numeric(levels(as.factor(inddat$italy$dat$year)))

# setup the prediction grid
ax<-c(it.inddat$x,sc.inddat$x)#,sa.inddat$x)
ay<-c(it.inddat$y,sc.inddat$y)#,sa.inddat$y)
itmat<-matrix(c(ax,ay),length(ax),2)
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
#   z<-predict(sa.soap,pred.grid,type="response")
#   im.it[inSide(sa,xx,yy)]<-z[!is.na(z)]

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
#   lines(sa,lwd=2)
   lines(sc,lwd=2)

}

