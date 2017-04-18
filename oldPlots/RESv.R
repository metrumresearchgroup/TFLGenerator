RESv <-
function(datFile, groupBy=NULL, xBy="IPRE", yBy="CWRES", 
									 markBy=NULL,
									 xLimit=NULL, yLimit=NULL,
									 xForm=waiver(), yForm=waiver(),
									 xScale="log10", yScale="identity", 
									 Title="", xLab="Conditional Weighted Residuals", yLab="Individual Prediction",
									 facetBy="", 
         minorTicks=NULL,minorTickNum=10,
         themeUpdate=list(),
         themeTextSize=14,
         themePlotTitleSize=1.2,
         themeAxisTxtSize=0.8,
         themeAxisTxtColour='black',
         themeAxisTitleTxtSize=0.9,
         themeAxisTitleColour='black',
         themePanelBackgroundFill='white',
         themePanelGridSize=NULL,
         themePanelGridColour='white',
         themePanelLineType=1,
         themePanelTitleSize=1.2,
         themePlotTitleColour='black',
									 ...)
{

	p1=ggplot(datFile, aes_string(x=xBy, y=yBy, group=groupBy, color=markBy, lty=markBy))+
		geom_point(shape=1)+
		#cleanTheme+
		cleanScales+
		geom_smooth(colour="red", lty=2, se=FALSE)+
		scale_y_continuous(limits=yLimit, labels=eval(yForm), trans=yScale)+
		scale_x_continuous(labels=eval(xForm), limits=xLimit, trans=xScale)+
		geom_abline(intercept=0, slope=0)+
		labs(title=as.character(Title), x=xLab, y=yLab)
	
	if (!is.null(minorTicks)) p1=p1+annotation_ticks(ticks_per_base = minorTickNum,sides = minorTicks)
	
	
	#Add in the faceting if it exists
	if (facetBy!=""){
		p1=p1 +facet_wrap(as.formula(paste("~", facetBy)))
	}
	
	rel=ggplot2:::rel
	themeUpdate=theme(text=              element_text(size=themeTextSize),
	                  axis.text =        element_text(size=rel(themeAxisTxtSize),colour = themeAxisTxtColour),
	                  axis.title =       element_text(size=rel(themeAxisTitleTxtSize),colour = themeAxisTitleColour),
	                  plot.title =       element_text(size=rel(themePlotTitleSize),colour=themePlotTitleColour),
	                  panel.background = element_rect(fill = themePanelBackgroundFill),
	                  panel.grid.major=  element_line(size=themePanelGridSize,colour=themePanelGridColour,linetype=themePanelLineType)
	)
	
	p1=p1+cleanTheme +themeUpdate
	p1=list(pList=list(p1),plotCols=1,plotRows=1)
	class(p1)<-c(class(p1),'TFL')	
	return(p1)
	
}
