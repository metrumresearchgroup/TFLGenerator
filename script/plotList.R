gitDir <- getwd()

plotList <- list(
  type = c(
     "ConcvTime",      "ConcvTimeGroup", "demogTab"     ,   "OBSvPRED",
     "paramDist",      "covCat"        , "covCon"       ,   "corPairs",
     "QQplot"   ,      "NMTab"         , "ConcvTimeMult",   "DescParamStat" 
  ),
  Ind = c(
    "ConcvTime" ,    "ConcvTimeInd"   , "demogTab"      ,   "OBSvIPRED"  ,
    "paramDist" ,    "covCat"         , "covCon"        ,   "ggpairs"    ,
    "QQplot"    ,    "RNM"            , "ConcvTimeMult" , "DescParamStat"
  ),
  Sum = c(
    "ConcvTime" ,    "ConcvTimeSum"   ,  "demogTab"     , "OBSvIPRED"    ,
    "paramDist" ,    "covCat"         ,  "covCon"       , "ggpairs"      ,
    "QQplot"    ,    "RNM"            , "ConcvTimeMult" , "DescParamStat"
  ),
  Call = c(
    "panelTypeConcvTime"  ,    "panelTypeConcvTimeGroup" , "panelTypedemogTab"     ,  "panelTypeOBSvPRED"      ,
  "panelTypeparamDist"  ,    "panelTypecovCat"         , "panelTypecovCon"        , "panelTypecorPairs"       ,
  "panelTypeQQplot"     ,    "panelTypeNMTab"          , "panelTypeConcvTimeMult" , "panelTypeDescParamStat"  
  )
)

save(plotList,file=file.path(gitDir,"GUI","data","plotList.rda")) # Expects to run from top of repo
