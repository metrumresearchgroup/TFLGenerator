gitDir <- getwd()
require(ggplot2)
require(scales)

for(x in list.files(file.path(gitDir,'TFL/data'),pattern = 'Palette',full.names = T)){
 load(x) 
}

cleanScales=list(ggplot2::scale_fill_manual(values=colorPalette,drop=T), 
                 ggplot2::scale_shape_manual(values=scales::shape_pal()(6),drop=T), 
                 ggplot2::scale_linetype_manual(values=colorlinePalette,drop=T),
                 ggplot2::scale_colour_manual(values=colorPalette,drop=T))

save(cleanScales,file=file.path(gitDir,"TFL","data","cleanScales.rda")) # Expects to run from top of repo