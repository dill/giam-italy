# Main model, spatiotemporal soap film smooth
# smooth Italy, Sardinia and Sicily...

# first load some libraries
library(maps)
library(mapdata)
library(soap)
library(dillhandy)
#library(ggplot2)

# extra scripts - to re-organise the data
source("fixit.R")

# run fixdata anyway to get the boundaries
full<-read.csv(file="database_complete.csv")
fixdat<-fix_it_data(full)

# if we want to use Gamma distribution then add a small number
# to the data, don't need to if Tweedie errors
eps<-0 # if Tweedie
#eps<-1e-8 # if Gamma 

# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)
# Sardinia boundary
sa<-list(x=fixdat$sardinia$map$km.e,y=fixdat$sardinia$map$km.n)
# Sicily boundary
sc<-list(x=fixdat$sicily$map$km.e,y=fixdat$sicily$map$km.n)

########################
# Italy

# form the data set
it.dat<-list(x=fixdat$italy$dat$km.e,
          y=fixdat$italy$dat$km.n,
          year=fixdat$italy$dat$year,
          share_100=fixdat$italy$dat$share_100+eps)

# basis size
it.bsize<-c(20,6)
# setup the soap knots
soap.knots.it<-make_soap_grid(it,c(14,14))
soap.knots.it<-pe(soap.knots.it,-c(25)) #14 x14

########################
# Sardinia 

# form the data set
sa.dat<-list(x=fixdat$sardinia$dat$km.e,
          y=fixdat$sardinia$dat$km.n,
          year=fixdat$sardinia$dat$year,
          share_100=fixdat$sardinia$dat$share_100+eps)

soap.knots.sa<-make_soap_grid(sa,c(5,6))

sa.ksize<-c(8,6)

########################
# Sicily 

# form the data set
sc.dat<-list(x=fixdat$sicily$dat$km.e,
          y=fixdat$sicily$dat$km.n,
          year=fixdat$sicily$dat$year,
          share_100=fixdat$sicily$dat$share_100+eps)

# setup the soap knots
soap.knots.sc<-make_soap_grid(sc,c(6,6))

sc.bsize<-c(10,6)

big.dat<-rbind(as.data.frame(it.dat),
               as.data.frame(sa.dat),
               as.data.frame(sc.dat))
big.dat<-as.data.frame(big.dat)

big.dat<-big.dat[big.dat$year==2008,]

postscript(file="pointmap.eps",width=7,height=8)

par(mar=c(4.5,4.5,2,2))

plot(big.dat$x,big.dat$y,asp=1,pch=19,cex=0.1,
     xlab="km (e)",ylab="km (n)",
     cex.main=1.4,cex.lab=1.4,cex.axis=1.3)

lines(it,lwd=2)
lines(sa,lwd=2)
lines(sc,lwd=2)


dev.off()
