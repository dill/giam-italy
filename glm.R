# try a simple glm model...

source("models.R")
source("pe.R")
full<-read.csv(file="database/database_complete.csv")
fixdat<-fix_it_data(full)

it.dat<-fixdat$italy$dat


