library(dplyr)
dataDir='../TFL/inst/external/0069'
tab=read.table(file.path(dataDir,"0069.tab"), header=TRUE, skip=1, stringsAsFactors=F, fill=TRUE)
tab <- subset(tab, select=c(ID:HGTB,MDV))
tab <- group_by(tab, ID,TIME,STUD,EVID)
tab <- mutate(tab, WGT=WGTB+.1*TIME+rnorm(length(WGTB)))
tab$WGT[ tab$STUD==183 ] <- NA
write.csv(tab, file=file.path(dataDir,'0069.csv'),row.names=F)
