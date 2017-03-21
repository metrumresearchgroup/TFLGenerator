gitDir <- getwd()
require(ggplot2)
require(scales)

fillList=NULL
shapeList=NULL
lineList=NULL
colourList=NULL
drop=T

cPal=c(
   "darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"   
   ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"  
   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"     
   ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"     
   ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"    
   ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"     
   ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen"
   ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"    
   ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"    
   ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"   
   ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"          
   ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"   
   ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"  
   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"     
   ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"     
   ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"    
   ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"     
   ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen"
   ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"    
   ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"    
   ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"   
   ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"          
   ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"   
   ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"  
   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"           ,"darkolivegreen" ,"firebrick"     
   ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"     ,"aquamarine1"    ,"violetred1"     ,"slateblue"     
   ,"plum"           ,"darkolivegreen" ,"firebrick"      ,"dodgerblue4"    ,"goldenrod"      ,"springgreen4"   ,"darkorchid"     ,"chocolate1"    
   ,"aquamarine1"    ,"violetred1"     ,"slateblue"      ,"plum"
)

chk=function (a, b) 
{
  if (!is.null(a)) 
    a
  else b
}

fillList=chk(fillList,cPal)
shapeList=chk(shapeList,scales::shape_pal()(6))
lineList=chk(lineList,rep(1,60))
colourList=chk(colourList,cPal)

cleanScales=list(ggplot2::scale_fill_manual(values=fillList,drop=drop), 
                 ggplot2::scale_shape_manual(values=shapeList,drop=drop), 
                 ggplot2::scale_linetype_manual(values=lineList,drop=drop),
                 ggplot2::scale_colour_manual(values=colourList,drop=drop))

save(cleanScales,file=file.path(gitDir,"TFL","data","cleanScales.rda")) # Expects to run from top of repo