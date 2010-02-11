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

   if(!is.list(bnd[[1]])){
      xmin<-min(bnd$x)
      ymin<-min(bnd$y)
      xmax<-max(bnd$x)
      ymax<-max(bnd$y)
   }else{
      # handles the case with multiple boundaries where bnd is a list of lists
      xmin<-min(unlist(pe(bnd,"x")))
      ymin<-min(unlist(pe(bnd,"y")))
      xmax<-max(unlist(pe(bnd,"x")))
      ymax<-max(unlist(pe(bnd,"y")))
   }

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

