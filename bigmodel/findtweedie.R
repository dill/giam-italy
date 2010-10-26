# find the parameter for the Tweedie -- brute force style


library(foreach)
library(doMC)
options(cores=6)
registerDoMC()

library(maps)
library(mapdata)
library(soap)
library(dillhandy)

# extra scripts
source("fixit.R")
source("diagnostics.R")

# run fixdata anyway to get the boundaries
full<-read.csv(file="database_complete.csv")
fixdat<-fix_it_data(full)

eps<-0 # if Tweedie

# Italy boundary
it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)

# data
it.dat<-list(x=fixdat$italy$dat$km.e,
          y=fixdat$italy$dat$km.n,
          year=fixdat$italy$dat$year,
          share_100=fixdat$italy$dat$share_100+eps)

# basis size
it.bsize<-c(20,6)
# setup the soap knots
soap.knots.it<-make_soap_grid(it,c(15,15))
soap.knots.it<-pe(soap.knots.it,-c(1,46)) #15 x15
#soap.knots.it<-make_soap_grid(it,c(15,20))
#soap.knots.it<-pe(soap.knots.it,-c(7,20,33,66,69)) #15 x15

paropts<-seq(1.4,1.95,by=0.05)

results<-foreach(ind = 1:length(paropts), .combine=rbind,
                   .inorder=FALSE, .init=c()) %dopar% {

   tweediepar<-paropts[ind]
   
   it.soap<- gam(share_100~
      te(x,y,year,bs=c("sf","cr"),k=it.bsize,d=c(2,1),xt=list(list(bnd=list(it)),NULL))+
      te(x,y,year,bs=c("sw","cr"),k=it.bsize,d=c(2,1),xt=list(list(bnd=list(it)),NULL))
            ,knots=soap.knots.it,data=it.dat,family=Tweedie(link=power(0),p=tweediepar),method="REML")
   gc()
   
   ## run some checks
   pdf(file=paste("check-Tweedie-",tweediepar,".pdf",sep=""),4,4)
   diagnostic(it.soap,res=10)
   dev.off()

   return(1)
}

