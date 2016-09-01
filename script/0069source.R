library(dplyr)
tab=read.table("NMStorage/0069/0069.tab", header=TRUE, skip=1, stringsAsFactors=F, fill=TRUE)
tab <- subset(tab, select=c(ID:HGTB,MDV))
tab <- group_by(tab, ID,TIME,STUD,EVID)
summary(tab)
tab <- mutate(tab, WGT=WGTB+.1*TIME+rnorm(length(WGTB)))
tab$WGT[ tab$STUD==183 ] <- NA
write.csv(tab, file="NMStorage/0069/source.csv",row.names=F)
