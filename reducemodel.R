source("models.R")

# load full data
full<-read.csv(file="database/database_complete.csv")

# set par
#par(mfrow=c(2,3))

# run the data formatter for the 2003 data set
#fixdat<-fix_it_data(it2003)
#mod_2003<-run_altmods(fixdat,150,plot.it=FALSE, year="(2003)")

fixdat<-fix_it_data(full)


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
                                   fixdat$sardinia$dat$altimetry),
                       year=c(fixdat$italy$dat$year,
                                   fixdat$sicily$dat$year,
                                   fixdat$sardinia$dat$year))
zlim<-c(0,12)

# plot the raw data
eda_rets<-do_eda(fullll,plot.it=plot.it,zlim=zlim,year="",dat.ret=TRUE)

it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

soap.knots<-make_soap_grid(list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n),c(20,30))
soap.knots<-pe(soap.knots,-c(11,32,47,121))



fulldat<-eda_rets$dat
fulldat$share_100<-fulldat$share_100+1e-10

b.soap<- gam(share_100~
      s(x,y,bs=c("so"),k=c(100),xt=list(bnd=list(it)))
               ,knots=soap.knots,data=fulldat,family=Gamma(link="log"))









