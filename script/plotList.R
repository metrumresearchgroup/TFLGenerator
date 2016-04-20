gitDir <- getwd()

plotList <- list(
  type = c(
     "ConcvTime",      "ConcvTimeGroup", "demogTab"     ,   "OBSvPRED",
     "paramDist",      "covCat"        , "covCon"       ,   "corPairs",
     "QQplot"   ,      "NMTab"         , "ConcvTimeMult",   "DescParamStat",
     "demogTabCont",   "demogTabCat"   , "GOF"          ,   "inputTable"   ,
     "inputFigure",    "inputListing"  , "inputListing_text"
  ),
  Ind = c(
    "ConcvTime" ,    "ConcvTimeInd"   , "demogTab"      ,   "OBSvIPRED"  ,
    "paramDist" ,    "covCat"         , "covCon"        ,   "ggpairs"    ,
    "QQplot"    ,    "RNM"            , "ConcvTimeMult" , "DescParamStat",
    "demogTabCont",  "demogTabCat"    , "GOF"           , "inputFile"    ,
    "inputFile"   ,  "inputFile"      , "inputFile"
  ),
  Sum = c(
    "ConcvTime" ,    "ConcvTimeSum"   ,  "demogTab"     , "OBSvIPRED"    ,
    "paramDist" ,    "covCat"         ,  "covCon"       , "ggpairs"      ,
    "QQplot"    ,    "RNM"            , "ConcvTimeMult" , "DescParamStat",
    "demogTabCont",  "demogTabCat"    , "GOF"           , "inputFile"    ,
    "inputFile",     "inputFile"      , "inputFile"
  ),
  Call = c(
    "panelTypeConcvTime"      ,  "panelTypeConcvTimeGroup" , "panelTypedemogTab"      , "panelTypeOBSvPRED"         ,
  "panelTypeparamDist"        ,  "panelTypecovCat"         , "panelTypecovCon"        , "panelTypecorPairs"         ,
  "panelTypeQQplot"           ,  "panelTypeNMTab"          , "panelTypeConcvTimeMult" , "panelTypeDescParamStat"    ,
  "panelTypedemogTabCont"     ,  "panelTypedemogTabCat"    , "panelTypeGOF"           , "panelTypeinputFile_image"  ,
  "panelTypeinputFile_image"  ,  "panelTypeinputFile_image", "panelTypeinputFile"
  ),
  sidebarType = 
    c("Figures"               , "Figures"                 , "Tables"                 , "Figures"                    ,
      "Figures"               , "Figures"                 , "Figures"                , "Figures"                    , 
      "Figures"               , "Tables"                  , "Listings"               , "Tables"                     ,
      "Tables"                , "Tables"                  , "Figures"                , "Tables"                     ,
      "Figures"               , "Listings"                , "Listings" 
  )
)

save(plotList,file=file.path(gitDir,"GUI","data","plotList.rda")) # Expects to run from top of repo
