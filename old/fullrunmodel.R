source("models.R")

# load full data
full<-read.csv(file="database/database_complete.csv")

# set par
#par(mfrow=c(2,3))

# run the data formatter for the 2003 data set
#fixdat<-fix_it_data(it2003)
#mod_2003<-run_altmods(fixdat,150,plot.it=FALSE, year="(2003)")

fixdat<-fix_it_data(full)

plot.it=FALSE
source("fullmodel.R")

#mod_2008<-run_altmods(fixdat,30,plot.it=FALSE,year="(2008)")

#dev.copy(postscript,"filename",width=20,height=20)
#dev.off()
