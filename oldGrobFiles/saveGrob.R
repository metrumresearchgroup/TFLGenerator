#' @title Saves a graphic object to the guiGrob list (or creates a new list and saves it)
#' @description Saves the current graphic object to a list of graphic objects.  The list of objects (called 'guiGrobs') remains in the global environment and is updated upon this function call, the list is also saved to a *R document in the project directory
#' @usage saveGrob(plot, Name, file, fac = "", mar = "", gro = "", str = "", legtit = Name, leg = sprintf("Information about \%s", Name))
#' @param plot name (not character) of the plot object to save, can be list containing the elements required of the grobList
#' @param Name character name by which to record the grob
#' @param file character path location of the grob file
#' @param fac character faceting information
#' @param mar character marking information (for example, marked by Dose)
#' @param gro character grouping information (for example, grouped by Route)
#' @param str character stratification information (for example, stratified by Study) ---> currently not in use
#' @param legtit character string for the legend title
#' @param leg character string for the legend
#' @details Most of the details will be assigned automatically through the Gui, however, users can choose to enter them by hand.
#' @return returns nothing, saves to a file
#' @export

saveGrob <-
function(plot, Name, file,
									fac="", mar="", gro="", str="", legtit=Name, 
         leg=sprintf("Information about %s", Name), foot="",type="",writeRda=T,
         ...){
	
	fileTitle=file
	dots <- list(...)
	
	if(!file.exists(fileTitle)){
		#Add empty Grob List
		guiGrobs=list()
		save(guiGrobs, file=fileTitle)
		
	}
	
	#if the grobs aren't loaded already, load them in
	if(!exists("guiGrobs", envir=.GlobalEnv)){
		load(file=fileTitle, envir=.GlobalEnv, verbose=TRUE)
	}
	
	#Now append the new Grob 
	nn=length(guiGrobs)
	if (Name==""){saveName=nn}
	#convert the items to a list of the proper order
	if ("list" %nin% class(plot) | all(names(plot)=="src")){
		plot=list(Facets=fac, Marks=mar, Groups=gro, Stratification=str, 
		          LegendTitle=legtit, Legend=leg, Footnote=foot, Plot=plot,
		          Type=type)
	}else if("list" %in% class(plot) & "Type" %nin% names(plot)){
	  plot=c(plot,Facets=fac, Marks=mar, Groups=gro, Stratification=str, 
	         LegendTitle=legtit, Legend=leg, Footnote=foot, Plot=plot,
	         Type=type)
	}
	if("Footnote" %in% names(plot)){
	  if(plot$Footnote=="") plot$Footnote <- NULL
	}
	if(!is.null(dots$longText)) plot[["longText"]] <- dots$longText
	
	guiGrobs[[Name]]<<-plot
	
	# Trying to improve efficiency
	if(writeRda) save(guiGrobs, file=fileTitle)

}
