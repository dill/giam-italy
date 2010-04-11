# run the models from models.R 

source("models.R")

# first read in the csv for 2003 and 2008 for the whole of italy
it2003<-read.csv(file="database/database_2003.csv")
it2008<-read.csv(file="database/database_2008.csv")

# set par
par(mfrow=c(1,2))

# run the data formatter for the 2003 data set
fixdat<-fix_it_data(it2003)

fullll<-data.frame(lat= c(fixdat$italy$dat$latitude,
                          fixdat$sicily$dat$latitude,
                          fixdat$sardinia$dat$latitude),
                   long=c(fixdat$italy$dat$longitude,
                          fixdat$sicily$dat$longitude,
                          fixdat$sardinia$dat$longitude),
                   share_100=c(fixdat$italy$dat$share_100,
                               fixdat$sicily$dat$share_100,
                               fixdat$sardinia$dat$share_100))

ig<-do_eda(fullll,plot.it=TRUE,zlim=c(0,12),year="2003")

fixdat<-fix_it_data(it2008)


fullll<-data.frame(lat= c(fixdat$italy$dat$latitude,
                          fixdat$sicily$dat$latitude,
                          fixdat$sardinia$dat$latitude),
                   long=c(fixdat$italy$dat$longitude,
                          fixdat$sicily$dat$longitude,
                          fixdat$sardinia$dat$longitude),
                   share_100=c(fixdat$italy$dat$share_100,
                               fixdat$sicily$dat$share_100,
                               fixdat$sardinia$dat$share_100))

ig<-do_eda(fullll,plot.it=TRUE,zlim=c(0,12),year="2008")





#dev.copy(postscript,"filename",width=20,height=20)
#dev.off()
