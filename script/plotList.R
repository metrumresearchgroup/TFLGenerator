gitDir <- getwd()

plotList <- list(
  type = c(
     "ConcvTime",      "ConcvTimeGroup", 
     "OBSvPRED",
     "paramDist",      "covCat"        , "covCon"       ,   "corPairs",
     "QQplot"   ,      "NMTab"         , "ConcvTimeMult",   
     "demogTabCont",   "demogTabCat"   , "GOF"          ,   "inputTable"   ,
     "inputFigure",    "inputListing"  , "inputListing_text",
     "subjectExclusionsTab", "observationExclusionsTab",
     "subjectExclusionsSummaryTab",
     "observationExclusionsSummaryTab",
     "distMult",
     "barchartMult"
  ),
  Ind = c(
    "ConcvTime" ,    "ConcvTimeInd"   ,   
    "OBSvIPRED"  ,
    "paramDist" ,    "covCat"         , "covCon"        ,   "ggpairs"    ,
    "QQplot"    ,    "RNM"            , "ConcvTimeMult" , 
    "demogTabCont",  "demogTabCat"    , "GOF"           , "inputFile"    ,
    "inputFile"   ,  "inputFile"      , "inputFile",
    "subjectExclusionsTab", "observationExclusionsTab",
    "subjectExclusionsSummaryTab",
    "observationExclusionsSummaryTab",
    "distMult",
    "barchartMult"
  ),
  Sum = c(
    "ConcvTime" ,    "ConcvTimeSum"   ,  
    "OBSvIPRED"    ,
    "paramDist" ,    "covCat"         ,  "covCon"       , "ggpairs"      ,
    "QQplot"    ,    "RNM"            , "ConcvTimeMult" ,
    "demogTabCont",  "demogTabCat"    , "GOF"           , "inputFile"    ,
    "inputFile",     "inputFile"      , "inputFile",
    "subjectExclusionsTab", "observationExclusionsTab",
    "subjectExclusionsSummaryTab",
    "observationExclusionsSummaryTab",
    "distMult",
    "barchartMult"
  ),
  Call = c(
    "panelTypeConcvTime"      ,  "panelTypeConcvTimeGroup" ,
    "panelTypeOBSvPRED"         ,
  "panelTypeparamDist"        ,  "panelTypecovCat"         , "panelTypecovCon"        , "panelTypecorPairs"         ,
  "panelTypeQQplot"           ,  "panelTypeNMTab"          , "panelTypeConcvTimeMult" , 
  "panelTypedemogTabCont"     ,  "panelTypedemogTabCat"    , "panelTypeGOF"           , "panelTypeinputFile_image"  ,
  "panelTypeinputFile_image"  ,  "panelTypeinputFile_image", "panelTypeinputFile",
  "panelTypesubjectExclusionsTab", "panelTypeobservationExclusionsTab",
  "panelTypesubjectExclusionsSummaryTab",
  "panelTypeobservationExclusionsSummaryTab",
  "panelTypedistMult",
  "panelTypebarchartMult"
  ),
  sidebarType = 
    c("Figures"               , "Figures"                 , 
      "Figures"               ,
      "Figures"               , "Figures"                 , "Figures"                , "Figures"                    , 
      "Figures"               , "Tables"                  , "Listings"               , 
      "Tables"                , "Tables"                  , "Figures"                , "Tables"                     ,
      "Figures"               , "Listings"                , "Listings",
      "Listings"              , "Listings",
      "Tables"                ,
      "Tables"                ,
      "Figures"               ,
      "Figures"
  )
)

save(plotList,file=file.path(gitDir,"GUI","data","plotList.rda")) # Expects to run from top of repo
