#' @title Indexes currently available Grobs
#' @description After production of a grob list with saveGrob, indexes all produced grobs
#' @usage indexGrobs()
#' @details Shows grob details, including figure title and figure legend text, if assigned
#' @return Print statement to the console
#' @export

indexGrobs <-
function(){
	for(n in (1:length(guiGrobs))){
		print(paste(n, ":", names(guiGrobs)[n]))
		if(guiGrobs[[n]][5]!=""){
			print(paste("   ", guiGrobs[[n]][5]))
		}
		if(guiGrobs[[n]][6]!=""){
			print(paste("   ", guiGrobs[[n]][6]))
		}	
		if(guiGrobs[[n]][1]!=""){
			print(paste("  Faceted by:", guiGrobs[[n]][1]))
		}
		print(paste("  Grouped by:", guiGrobs[[n]][3]))
		print(paste("  Marked by:", guiGrobs[[n]][2]))
		if(guiGrobs[[n]][4]!=""){
			print(paste("\tStratified by:", guiGrobs[[n]][4]))
		}
		print("")		
	}
}
