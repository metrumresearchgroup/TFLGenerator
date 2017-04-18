#' @export 
revealGrob <-
function(Name, printLegend=TRUE){
	if(printLegend){
		cat(guiGrobs[[Name]]$LegendTitle)
		cat("\n")
		cat(guiGrobs[[Name]]$Legend)
	}
	p1=guiGrobs[[Name]]$Plot	
	return(p1)
	
	
}
