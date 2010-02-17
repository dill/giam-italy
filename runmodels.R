# run the models from models.R 

source("models.R")

# first read in the csv for 2003 and 2008 for the whole of italy
it2003<-read.csv(file="database/database_2003.csv")
it2008<-read.csv(file="database/database_2008.csv")

# set par
par(mfrow=c(2,3))

# run the data formatter for the 2003 data set
fixdat<-fix_it_data(it2003)
mod_2003<-run_mods(fixdat,10,plot.it=TRUE, year="(2003)")

fixdat<-fix_it_data(it2008)
mod_2008<-run_mods(fixdat,10,plot.it=TRUE,year="(2008)")

#dev.copy(postscript,"filename",width=20,height=20)
#dev.off()
