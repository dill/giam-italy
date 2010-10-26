# boxplot diagnostics

library(soap)

load("fullmod-Gamma.RData")


# first generate the grid...
res.x<-res.y<-res<-100

it<-list(x=fixdat$italy$map$km.e,y=fixdat$italy$map$km.n)
xmax<-max(it$x)
ymax<-max(it$y)
xmin<-min(it$x)
ymin<-min(it$y)
delx<-(xmax-xmin)/res
dely<-(ymax-ymin)/res



# grab the residuals
resids<-residuals(it.soap,type="deviance")
#resids<-residuals(it.soap,type="pearson")

# find the grid cells the residuals lie in...
xi<-abs(floor((it.dat$x-xmin)/delx))+1
yj<-abs(floor((it.dat$y-ymin)/dely))+1




# plot them

par(mfrow=c(1,3))


# x data
xdata<-data.frame(ind=xi,resids=resids)
boxplot(resids~ind,data=xdata,main="x")

# y data
ydata<-data.frame(ind=yj,resids=resids)
boxplot(resids~ind,data=ydata,main="y")

# year data
yeardata<-data.frame(ind=it.dat$year,resids=resids)
boxplot(resids~ind,data=yeardata,main="year")




