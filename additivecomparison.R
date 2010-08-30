# read in av data and run the model
# this is the big fat version that does Italy, Sardinia and Sicily...

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
av.dat<-read.csv(file="av.full.dat")

av.dat$year<-as.numeric(av.dat$year)
#av.dat$share_100<-av.dat$share_100+1e-15
av.dat$share_100<-av.dat$share_100+1e-5

load("REMLfull.RData")

cat("########### soap ##############\n")
cat("Italy AIC=",AIC(it.soap),"\n")
cat("Sardinia AIC=",AIC(sa.soap),"\n")
cat("Sicily AIC=",AIC(sc.soap),"\n")


cat("Italy BIC=",AIC(it.soap),"\n")
cat("Sardinia BIC=",AIC(sa.soap),"\n")
cat("Sicily BIC=",AIC(sc.soap),"\n")






########################
# Italy

# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

# setup the soap knots
#20x25
soap.knots<-make_soap_grid(it,c(20,25))
soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108))

onoff<-inSide(it,av.dat$x,av.dat$y)

av.dat.it<-pe(av.dat,onoff)

it.ad<- gam(share_100~
   s(x,y,bs="so",k=50,xt=list(bnd=list(it)))+
   s(year,bs="cr",k=4,xt=list(bnd=list(it)))
            ,knots=soap.knots,data=av.dat.it,family=Gamma(link="log"),method="REML")
##########################



########################
# Sardinia 

# Sardinia boundary
sa<-list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n)

# setup the soap knots
#20x25
soap.knots<-make_soap_grid(sa,c(10,10))
soap.knots<-pe(soap.knots,-c(47))

onoff<-inSide(sa,av.dat$x,av.dat$y)

av.dat.sa<-pe(av.dat,onoff)

sa.soap<- gam(share_100~
   s(x,y,bs="so",k=20,xt=list(bnd=list(sa)))+
   s(year,bs="cr",k=4,xt=list(bnd=list(sa)))
            ,knots=soap.knots,data=av.dat.sa,family=Gamma(link="log"),method="REML")
##########################



########################
# Sicily 

# Sicily boundary
sc<-list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n)

# setup the soap knots
#20x25
soap.knots<-make_soap_grid(sc,c(10,10))
#soap.knots<-pe(soap.knots,-c(4,5,11,35,61,68,108))

onoff<-inSide(sc,av.dat$x,av.dat$y)

av.dat.sc<-pe(av.dat,onoff)

sc.soap<- gam(share_100~
   s(x,y,bs="so",k=20,xt=list(bnd=list(sc)))+
   s(year,bs="cr",k=4,xt=list(bnd=list(sc)))
            ,knots=soap.knots,data=av.dat.sc,family=Gamma(link="log"),method="REML")
##########################

# calculate AIC/BIC

cat("########### additive ##############\n")
cat("Italy AIC=",AIC(it.ad),"\n")
cat("Sardinia AIC=",AIC(sa.ad),"\n")
cat("Sicily AIC=",AIC(sc.ad),"\n")


cat("Italy BIC=",AIC(it.ad),"\n")
cat("Sardinia BIC=",AIC(sa.ad),"\n")
cat("Sicily BIC=",AIC(sc.ad),"\n")







