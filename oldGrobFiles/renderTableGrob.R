renderTableGrob=
	function(tableFrame){
		
		return(
			arrangeGrob(tableGrob(tableFrame,
														show.rownames=FALSE, rows=NULL,
														show.box = TRUE,separator = "white", 
														padding.h=unit(12, "mm"),padding.v=unit(4, "mm"),
														gpar.coltext=gpar(col="black", cex=.85),
														show.vlines = FALSE, show.hlines = FALSE, 
														gpar.coretext = gpar(col = "black", 	cex = 0.8), 
														gpar.corefill = gpar(fill = "white", col = "white"),
														gpar.rowfill = gpar(fill = "white", col = "white"),
														gpar.colfill = gpar(fill = "white", col = "white")
			)
			)
		)
		
		
		
	}