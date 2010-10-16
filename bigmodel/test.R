library(soap)

# uses the example from soap-package

n<-10000
x <- runif(n)*10-5;y<-runif(n)*2-1
t <- runif(n)
z <- fs.test(x,y,b=1)
z <- z + 4.2
z <- z^(.5+t)
bnd<-fs.boundary()

# make two horeshoes
bnd<-list(list(x=bnd$x+1,y=bnd$y),
          list(x=-(bnd$x+1),y=-bnd$y))

# find what's inside
ind <- inSide(bnd,x=x,y=y) ## remove outsiders
z <- z[ind];x <- x[ind]; y <- y[ind]; t <- t[ind] 
n <- length(z)
z <- z + rnorm(n)*.05 ## add noise

# knots
m<-10;n<-5
xm <- seq(-5,5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
knots<-list(x=xx[onoff],y=yy[onoff])

# run the model
bk <- gam(z~ 
 te(x,y,t,bs=c("sf","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=bnd),NULL))+
 te(x,y,t,bs=c("sw","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=bnd),NULL))
          ,knots=knots)

