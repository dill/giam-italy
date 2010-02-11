# function to create a grid for soap
make_soap_grid<-function(bnd,n.grid){

   require(soap)

   # set the grid size, if the input is a 2-vec then it is m and n
   if(length(n.grid)==2){
      m<-n.grid[1]
      n<-n.grid[2]
   }else{
      m<-n<-n.grid
   }

   # min and max values of the boundary
   xmin<-min(bnd$x,na.rm=TRUE)
   ymin<-min(bnd$y,na.rm=TRUE)
   xmax<-max(bnd$x,na.rm=TRUE)
   ymax<-max(bnd$y,na.rm=TRUE)

   # create the grid
   xm <- seq(xmin,xmax,length=m)
   yn<-seq(ymin,ymax,length=n)
   xx <- rep(xm,n)
   yy<-rep(yn,rep(m,n))

   # knock out the points not inside the bnd
   onoff<-inSide(bnd,xx,yy)
   xx<-xx[onoff]
   yy<-yy[onoff]

   return(list(x=xx,y=yy))

}

