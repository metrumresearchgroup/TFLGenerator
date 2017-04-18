#' @importFrom reshape2 melt dcast
#' @importFrom plyr ddply
DescParamStat <-
function(datFile, group="STUDY", subgroup=NULL, idVar="NMID",
					 statList=c("n", "Mean", "Median", "CV", "Min", "Max"),
					 paramList=list(c("Cmax", "unit"), c("tmax", "unit")),
						sigDig=3)
		{
		
		params=sapply(paramList, function(x){x[[1]][1]})
		units=sapply(paramList, function(x){x[[2]][1]})
		subData=datFile[!duplicated(datFile[c(idVar, group)]),]
		subData=subData[,c(group,subgroup,params)]
		
		subData=reshape2::melt(subData, id.vars=c(group,subgroup), variable_name="Parameter")
		
		
		# New version of length which can handle NA's: if na.rm==T, don't count them
		length2 <- function (x, na.rm=TRUE) {
			if (na.rm) sum(!is.na(x))
			else       length(x)
		}
		
		statData=plyr::ddply(subData, .variables=c(group, subgroup, "Parameter"),
									 .fun= function(xx) {
									 	c( n     	= length2(xx[,"value"], na.rm=TRUE),
									 		 Mean= signif(mean(xx[,"value"], na.rm=TRUE), digits=sigDig),
 								 		 Median=signif(median(xx[,"value"], na.rm=TRUE), digits=sigDig),
								 		 CV=signif((sd(xx[,"value"], na.rm=TRUE)/mean(xx[,"value"], na.rm=TRUE)*100),digits=sigDig),
 									 		 Min=signif(min(xx[,"value"], na.rm=TRUE),digits=sigDig),
									 		 Max=signif(max(xx[,"value"], na.rm=TRUE),digits=sigDig)
									 	)
									 	
									 } )
		
		statData=statData[,c(group, subgroup, statList, "Parameter")]
		dummy=data.frame(cbind("Parameter"=params, "Units"=units), stringsAsFactors=FALSE)
		
		statData=reshape2::melt(statData, id.vars=c("Parameter", subgroup, group), measure.vars=statList, variable_name="DS")
		for(i in c(1:length(statData$value))){
			statData$value[i]=ifelse(statData$DS[i]=="n",as.character(as.integer(statData$value[i])), as.character(signif(as.numeric(statData$value[i]), digits=sigDig)))
		}

		if(is.null(subgroup)){
		statData=reshape2::dcast(statData, formula=as.formula(sprintf("Parameter+DS~%s", group)), value="value")
		statData=data.frame(statData[order(statData$"Parameter"),], stringsAsFactors=FALSE, check.names=FALSE)
		statData$Parameter=as.character(statData$Parameter)
		
		statData$Parameter[duplicated(statData[,c(subgroup, "Parameter")])]=sapply(statData$Parameter[duplicated(statData[,c(subgroup, "Parameter")])],
																																							 function(x) dummy$Units[dummy$Parameter==x])
			
		statData$Parameter[duplicated(statData[,c(subgroup, "Parameter")])]=""
		
		}
		
		if(!is.null(subgroup)){
			statData=reshape2::dcast(statData, formula=as.formula(sprintf("%s+Parameter+DS~%s", subgroup, group)), value="value")
			statData=data.frame(statData[order(statData[,subgroup], statData$"Parameter"),], stringsAsFactors=FALSE, check.names=FALSE)
			statData$Parameter=as.character(statData$Parameter)
		
		statData[,subgroup]=as.character(statData[,subgroup])
			statData$Parameter[duplicated(statData[,c(subgroup, "Parameter")])]=sapply(statData$Parameter[duplicated(statData[,c(subgroup, "Parameter")])],
																																								 function(x) dummy$Units[dummy$Parameter==x])
		statData$Parameter[duplicated(statData[,c(subgroup, "Parameter")])]=""

		idx=rev(as.numeric(rownames(statData)[!duplicated(statData[,subgroup])]))
		for(i in idx){
			dummy=statData[i,]
			dummy=data.frame(apply(dummy, c(1,2), function(i){paste("")}), stringsAsFactors=FALSE, check.names=FALSE)
			dummy$Parameter=statData[i,subgroup]
			if(i!=1){
			statData=rbind(statData[1:i-1,], dummy, statData[i:length(statData$Parameter),])
			}
			if(i==1){
				statData=rbind(dummy, statData)
			}
		}
			statData[,subgroup]=NULL
		}
		
		statData=rename(statData, c("DS"="Descriptive\nStatistics"))
		p1=renderTableGrob(statData)
		
		return(p1)
	}
