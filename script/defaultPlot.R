defaultPlot=list(Shapes=NULL,Ratio=NULL)

defaultPlot$Shapes=rbind(data.frame(plot=c("OBSvPRED",
                                           "paramDist",
                                           "covCat",
                                           "covCon",
                                           "corPairs",
                                           "distMult",
                                           "barchartMult",
                                           "QQplot"),
                                    shape=1,
                                    stringsAsFactors = F),
                         data.frame(plot=c("ConcvTime",
                                           "ConcvTimeGroup",
                                           "VPC"),
                                    shape=2,
                                    stringsAsFactors = F),
                         data.frame(plot=c("GOF",
                                           "ConcvTimeMult"),
                                    shape=3,
                                    stringsAsFactors = F)
)

defaultPlot$Shapes$shape=as.character(factor(defaultPlot$Shapes$shape,labels=c('squares','rectangles','panel')))

defaultPlot$Ratio=data.frame(shape=c('squares','rectangles','panel'),height=c(6,4,7),scale=c(1,1.5,0.75),stringsAsFactors = F)

save(defaultPlot,file=file.path(getwd(),"GUI","data","defaultPlot.rda")) # Expects to run from top of repo
