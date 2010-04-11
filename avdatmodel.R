# read in av data and run the model

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

# run fixdata anyway to get the boundaries
full<-read.csv(file="database/database_complete.csv")
fixdat<-fix_it_data(full)

# load averaged data
av.dat<-read.csv(file="av.dat")

# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

# setup the soap knots
soap.knots<-make_soap_grid(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),c(20,30))
soap.knots<-pe(soap.knots,-c(11,32,47,121))

onoff<-inSide(it,av.dat$x,av.dat$y)

av.dat<-pe(av.dat,onoff)

# run a model!
b.soap<- gam(share_100~
   te(x,y,year,bs=c("sf","cr"),k=c(100,4),d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
   te(x,y,year,bs=c("sw","cr"),k=c(100,4),d=c(2,1),xt=list(list(bnd=list(it)),NULL))
            ,knots=soap.knots,data=av.dat,family=Gamma(link="log"))


