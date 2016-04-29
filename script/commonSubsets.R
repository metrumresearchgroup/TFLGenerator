#SUBSETS
subset($DATA, !is.na(DV) & DV!="." & TAFD>=0 & CMTFLAG%in%c(1))

#EXCLUSIONS
SUBJEXC = "Keep"
SUBJEXC[ RACE==88 ] = "Missing race"
OBSEXC = "Keep"
OBSEXC[ EVID==0 & DV<0.05] = "BQL"