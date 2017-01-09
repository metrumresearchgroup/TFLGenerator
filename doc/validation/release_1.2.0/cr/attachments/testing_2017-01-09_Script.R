#Use pkgSetup.R to install the pkg repository into ./lib


#Project~ AMG NO
#R Script Recorded from GUI
#on 2017-01-09
#


#For Help with active grobs see:
																													
#?indexGrobs
#?revealGrobs
#?loadGrobs

#--------------------------------------

# GUI version: 1.2.0
# TFL version: 1.2.0



########
#Initial Global Parameters
 #Project Directory
projDir='/data/NMStorage/mi210/AMG_NO_2017-01-09/' #Create Directory (no warning if directory exists)
dir.create(projDir, showWarnings=FALSE) 
#Project File Header
projHead='/data/NMStorage/mi210/AMG_NO_2017-01-09/testing_2017-01-09' 
#Grob File
grobFile='/data/NMStorage/mi210/AMG_NO_2017-01-09/testing_2017-01-09_Grobs.rda'
Sys.setenv(PATH=paste0(Sys.getenv("PATH"),":/usr/bin")) # Need imagemagick (in /usr/bin)
 .libPaths('/data/tflgenerator/script/lib')
 library(GUI)
 library(TFL)
 library(DT)             
 library(animation)      
 library(lazyeval)       
 library(gtools)         
 library(readr)          
 library(Hmisc)          
 library(ggplot2)        
 library(gridExtra)      
 library(colourpicker)   
 library(plyr)           
 library(dplyr)          
 library(shiny)          


#PNG Folder
pngFolder='/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/' 
dir.create(pngFolder, recursive=T)


#Color Pallette, shapes and lines for Plotting
cleanScales<-setColorScale(shapeList = shape_pal()(6))

########
#Current working directory
 currentWD <- '/data/NMStorage/mi210'
tabledat=matrix()
 foo <- read.table("/data/NMStorage/mi210/510/510.tab", header=TRUE, skip=1, stringsAsFactors=F)
 foo$Run=510
 tabledat=merge(tabledat, foo, all=TRUE)
 tabledat=tabledat[rowSums(is.na(tabledat)) != ncol(tabledat),]
 tabledat$V1=NULL
 tabledat=data.frame(tabledat, stringsAsFactors=F)
 tableNA <- c('')
 tableNAnum <- c()
 
		  for(j in 1:ncol(tabledat)){
		    if(class(tabledat[,j])=="numeric"){
		      tabledat[tabledat[,j]%in%tableNAnum,j] <- NA
		    }else{
		      tabledat[tabledat[,j]%in%tableNA,j] <- NA            
		    }
		  }
     
srcdat=as.best(read_csv("/data/NMStorage/mi210/poppk_wcovs.csv"))
 srcdat=srcdat[rowSums(is.na(srcdat)) != ncol(srcdat),]
 srcdat=data.frame(srcdat, stringsAsFactors=F)
 srcNA <- c('')
 srcNAnum <- c()
 
		  for(j in 1:ncol(srcdat)){
		    if(class(srcdat[,j])=="numeric"){
		      srcdat[srcdat[,j]%in%srcNAnum,j] <- NA
		    }else{
		      srcdat[srcdat[,j]%in%srcNA,j] <- NA            
		    }
		  }
     
if(exists("tabledat") & exists("srcdat")){ 
 dat <- merge(srcdat, tabledat, all=TRUE) 
 }else { 
 if(exists("tabledat")) dat <- tabledat else dat <- srcdat 
 }

 names(dat)[which(names(dat)=="DV")]= "DV"
 names(dat)[which(names(dat)=="TIME")]="TAFD"
 names(dat)[which(names(dat)=="ID")]="NMID"
 names(dat)[which(names(dat)=="")]="STUDY"
 names(dat)[which(names(dat)=="IPRED")]="IPRED"
 names(dat)[which(names(dat)=="PRED")]="PRED"
 if(all(c("DV","TAFD","NMID")%in%names(dat))){
 	if("STUDY" %in% names(dat)) dat <- dat[order(dat$STUDY,dat$NMID,dat$TAFD),] else dat <- dat[order(dat$NMID,dat$TAFD),] 
 }


 dat <- within(dat, {med <- median(CLCR[!duplicated(NMID)])})
 dat <- within(dat, {CLCRF <- CLCR <= med})
 dat <- within(dat, {CLCRF <- factor(CLCRF, levels=c(FALSE, TRUE), labels=c("<= med", "> med"))})
 dat <- within(dat, {med <- NULL})
 dat <- within(dat, {STUDY <- "Study 1"})

vpcDataList <- list()
 vpcDataList[["VPC1"]] <- GUI:::getvpcFile(n=1,
				project='/data/NMStorage/mi210',
				vpcRun='511',
				vpcColnames='ID, TIME, IPRED, DV, PRED, RES, WRES',
				vpcRep='1000',
				vpcSource='poppk_wcovs.csv',
				vpcSourceDV='DV',
				dataSubset=c(''),
				mergeKey=c('ID','TIME'),
				dataParse_vpc='',
				sortBy=TRUE)
 vpcDataList[["addlVPC1"]] <- GUI:::getvpcAddlFile(n=1,
				project='/data/NMStorage/mi210',
				addlVpcSource='<data.csv Data source of additional layer of observed points>',
				addlDataParse_vpc='',
				vpcSourceDV='DV',
				dataParse_vpc='')
# Plotting ----



#Plot of ConcvTime1 ----- 
# 
# 

 plotNum='ConcvTime1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat)

		ConcvTime1=ConcvTime(datFile=tempDat,
								xForm=comma,
								yForm=comma,
								ipredVar='IPRED',
								ID=1,
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								Title='A Plot Title',
								yLab='Serum Concentration'
					)


	print(ConcvTime1)
 
          ConcvTime1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","ConcvTime1")
                           
#Save the Plot
 do.call(pListSave,ConcvTime1)
 FigureTitle <- "Concentration time profiles"
 FigureCaption <- "Caption"
 Footnote <- "Footnote"
 Type="Figures"

 Plot <- ConcvTime1
 
#Update the Grob
saveGrob(Plot, Name='ConcvTime1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of ConcvTimeGroup1 ----- 
# Concentration time profiles 
# Caption 

 plotNum='ConcvTimeGroup1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								groupBy='SEX',
								markBy='SEX',
								dataLimits=list(commands=c("subset( datFile , DV > 0.05 )"), within=c(FALSE)),
								sumThis=TRUE,
								markByAdd=' mg/mL')

		ConcvTimeGroup1=ConcvTimeSum(datFile=tempDat,
								xForm=comma,
								yForm=comma,
								groupBy='SEX',
								markBy='SEX',
								Color=TRUE,
								fnrow='',
								fncol='',
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								Title='A Plot Title',
								yLab='Serum Concentration'
					)


	print(ConcvTimeGroup1)
 
          ConcvTimeGroup1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","ConcvTimeGroup1")
                           
#Save the Plot
 do.call(pListSave,ConcvTimeGroup1)
 FigureTitle <- "Concentration time summary"
 FigureCaption <- ""
 Footnote <- "Footnote"
 Type="Figures"

 Plot <- ConcvTimeGroup1
 
#Update the Grob
saveGrob(Plot, Name='ConcvTimeGroup1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of OBSvPRED1 ----- 
# 
# 

 plotNum='OBSvPRED1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								xBy='PRED',
								groupBy=NULL,
								markBy=NULL,
								facetBy='SEX')

		OBSvPRED1=OBSvIPRED(datFile=tempDat,
								xForm=comma,
								xScale='identity',
								xBy='PRED',
								facetBy='SEX',
								fnrow='',
								fncol='',
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								xLab='Population Prediction',
								yLab='Observed Serum Concentration'
					)


	print(OBSvPRED1)
 
          OBSvPRED1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","OBSvPRED1")
                           
#Save the Plot
 do.call(pListSave,OBSvPRED1)
 FigureTitle <- "Observed vs Population Predicted Serum Concentrations from <model>"
 FigureCaption <- ""
 Footnote <- "Footnote"
 Type="Figures"

 Plot <- OBSvPRED1
 
#Update the Grob
saveGrob(Plot, Name='OBSvPRED1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of paramDist1 ----- 
# 
# 

 plotNum='paramDist1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								groupBy=NULL,
								dataLimits=list(commands=c("subset( datFile , !duplicated(NMID) )"), within=c(FALSE)))

		paramDist1=paramDist(project='/data/NMStorage/mi210',
								datFile=tempDat,
								runno='510',
								xForm=waiver(),
								yForm=waiver(),
								fnrow='',
								fncol='',
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								xCols=c('ETA1','ETA2'),
								xLab=c('Eta (Cl)',' Eta(V)'),
								yLab='Density',
								nrow=2
					)


	print(paramDist1)
 
          paramDist1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","paramDist1")
                           
#Save the Plot
 do.call(pListSave,paramDist1)
 FigureTitle <- "Parameter distribution"
 FigureCaption <- ""
 Footnote <- "The y-axis shows the percent of observations that fall into respective bins.  The solid blue lines illustrate the density of the random effect distributions.  The red vertical lines show medians of these distributions.  The dashed red lines show the density of the random effect distributions as estimated by the model.  Shrinkage calculations are described in the Methods section."
 Type="Figures"

 Plot <- paramDist1
 
#Update the Grob
saveGrob(Plot, Name='paramDist1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of covCat1 ----- 
# 
# 

 plotNum='covCat1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								dataLimits=list(commands=c("subset( datFile , !duplicated(NMID) )"), within=c(FALSE)))

		covCat1=covCat(datFile=tempDat,
								yForm=waiver(),
								conList=list(c("ETA1", "ETA (CL)"),c("ETA2", "ETA (V)")),
								catList=c("SEX", "Gender"),
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000'
					)


	print(covCat1)
 
          covCat1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","covCat1")
                           
#Save the Plot
 do.call(pListSave,covCat1)
 FigureTitle <- "Distribution of <y variable(s)> Stratified by <x variable(s)>"
 FigureCaption <- ""
 Footnote <- "Footnote"
 Type="Figures"

 Plot <- covCat1
 
#Update the Grob
saveGrob(Plot, Name='covCat1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of covCon1 ----- 
# 
# 

 plotNum='covCon1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								dataLimits=list(commands=c("subset( datFile , !duplicated(NMID) )"), within=c(FALSE)))

		covCon1=covCon(datFile=tempDat,
								xForm=waiver(),
								yForm=waiver(),
								conList_x=list(c("AGE", "Age (years)"),c("WT", "Weight (kg)"),c("HT", "Height (cm)")),
								conList_y=list(c("ETA1", "Eta (Cl)"),c("ETA2", "Eta (V)")),
								themeTextSize=10.1,
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000'
					)


	print(covCon1)
 
          covCon1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","covCon1")
                           
#Save the Plot
 do.call(pListSave,covCon1)
 FigureTitle <- "Relationship between <y variable(s)> and Covariates <x variable(s)>"
 FigureCaption <- "Caption"
 Footnote <- "Footnote"
 Type="Figures"

 Plot <- covCon1
 
#Update the Grob
saveGrob(Plot, Name='covCon1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of corPairs1 ----- 
# 
# 

 plotNum='corPairs1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								dataLimits=list(commands=c("subset( datFile , !duplicated(NMID) )"), within=c(FALSE)))

		corPairs1=ggpairs(datFile=tempDat,
								xForm=waiver(),
								yForm=waiver(),
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								xCols=c('ETA1','ETA2'),
								xLab=c('Eta 1 (Cl)',' Eta 2 (V)')
					)


	print(corPairs1)
 
          corPairs1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","corPairs1")
                           
#Save the Plot
 do.call(pListSave,corPairs1)
 FigureTitle <- "Correlations of Interindividual Random Effects"
 FigureCaption <- "Caption"
 Footnote <- "The continuous covariates (points) and the red lowess (local regression smoother) trend lines are plotted.  The coefficient of determination between the covariates are also presented."
 Type="Figures"

 Plot <- corPairs1
 
#Update the Grob
saveGrob(Plot, Name='corPairs1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of QQplot1 ----- 
# 
# 

 plotNum='QQplot1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								xBy='ETA1',
								groupBy=NULL,
								markBy=NULL,
								dataLimits=list(commands=c("subset( datFile , !duplicated(NMID) )"), within=c(FALSE)))

		QQplot1=QQplot(datFile=tempDat,
								xForm=comma,
								yForm=comma,
								fnrow='',
								fncol='',
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								Title='Eta 1',
								xLab='Quantile of standard normal',
								yLab='Eta 1'
					)


	print(QQplot1)
 
          QQplot1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","QQplot1")
                           
#Save the Plot
 do.call(pListSave,QQplot1)
 FigureTitle <- "QQ plot"
 FigureCaption <- ""
 Footnote <- "Distribution of <random effect> is compared to the standard normal distibution."
 Type="Figures"

 Plot <- QQplot1
 
#Update the Grob
saveGrob(Plot, Name='QQplot1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of NMTab1 ----- 
# 
# 

 plotNum='NMTab1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat)

		NMTab1=RNM(project='/data/NMStorage/mi210',
								Run='510',
								View=FALSE,
								OffDiagonals=FALSE,
								datFile=tempDat
					)
Footnote <- "Footnote"
 NMTab1 <- renderTex(NMTab1,"NMTab1",tmpDir="/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/",footnote=Footnote)
 FigureTitle <- "Parameter Estimates from <model> "
 FigureCaption <- "The estimates and their corresponding precisions (%RSE) are those found from the population analysis."
 Footnote <- ""
 Type="Tables"

 Plot <- NMTab1["src"]
 
#Update the Grob
saveGrob(Plot, Name='NMTab1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of ConcvTimeMult1 ----- 
# 
# 

 plotNum='ConcvTimeMult1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat)

		ConcvTimeMult1=ConcvTimeMult(datFile=tempDat,
								xForm='plain',
								yForm='plain',
								Color=TRUE,
								ipredVar='IPRED',
								ID=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40),
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								Title='A Plot Title',
								yLab='Serum Concentration',
								nrow=4,
								doseVar='',
								doseLab='',
								genAll=TRUE
					)


	print(ConcvTimeMult1)
 
          lapply(1:length(ConcvTimeMult1), function(i){
            ConcvTimeMult1[[i]]$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/",paste0("ConcvTimeMult1","-",i))
            do.call(pListSave,ConcvTimeMult1[[i]])  
          })

 FigureTitle <- "Population (Solid Lines), Individual Model Fit (Dashed Lines), and Observations (Open Circles) for Subjects in Clinical Studies Using <Model>"
 FigureCaption <- ""
 Footnote <- ""
 Type="Listings"

 Plot <- ConcvTimeMult1
 
#Update the Grob
saveGrob(Plot, Name='ConcvTimeMult1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of demogTabCont1 ----- 
# 
# 

 plotNum='demogTabCont1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								groupBy=c("STUDY", "Study"))

		demogTabCont1=demogTabCont(datFile=tempDat,
								conList=list(c("AGE", "Age (years)"),c("HT", "Height (cm)"),c("WT", "Weight (kg)")),
								catList='',
								groupBy=c("STUDY", "Study")
					)
Footnote <- "Footnote"
 demogTabCont1 <- renderTex(demogTabCont1,"demogTabCont1",tmpDir="/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/",footnote=Footnote)
 FigureTitle <- "Summary statistics of <continuous columns> by <stratification>"
 FigureCaption <- "Caption"
 Footnote <- ""
 Type="Tables"

 Plot <- demogTabCont1["src"]
 
#Update the Grob
saveGrob(Plot, Name='demogTabCont1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of demogTabCat1 ----- 
# 
# 

 plotNum='demogTabCat1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								groupBy=c("STUDY", "Study"))

		demogTabCat1=demogTabCat(datFile=tempDat,
								catList=c("SEX", "Sex"),
								stratBy='',
								groupBy=c("STUDY", "Study")
					)
Footnote <- "Footnote"
 demogTabCat1 <- renderTex(demogTabCat1,"demogTabCat1",tmpDir="/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/",footnote=Footnote)
 FigureTitle <- "Summary statistics of <categorical columns> by <stratification>"
 FigureCaption <- "Caption"
 Footnote <- ""
 Type="Tables"

 Plot <- demogTabCat1["src"]
 
#Update the Grob
saveGrob(Plot, Name='demogTabCat1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of GOF1 ----- 
# 
# 

 plotNum='GOF1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								markBy=NULL)

		GOF1=GOF(datFile=tempDat,
								smooth=TRUE,
								themeTextSize=9.2,
								themePlotTitleSize=1.1,
								themeAxisTxtSize=1,
								themeAxisTxtColour='#000000',
								themeAxisTitleTxtSize=1,
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								npdeFmt='plain',
								cwresFmt='plain',
								cwresBy='WRES',
								ipredFmt='plain',
								predFmt='plain',
								concFmt='plain',
								timeFmt='plain'
					)


	print(GOF1)
 
          GOF1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","GOF1")
                           
#Save the Plot
 do.call(pListSave,GOF1)
 FigureTitle <- "Goodness of Fit Plots"
 FigureCaption <- ""
 Footnote <- "The red line is the lowess (local regression smoother) trend line"
 Type="Figures"

 Plot <- GOF1
 
#Update the Grob
saveGrob(Plot, Name='GOF1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of distMult1 ----- 
# 
# 

 plotNum='distMult1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								dataLimits=list(commands=c("subset( datFile , !duplicated(NMID) )"), within=c(FALSE)))

		distMult1=distMult(datFile=tempDat,
								conList=list(c("WT", "Weight (kg)"),c("AGE", "Age (years)"),c("HT", "Height (cm)"),c("CLCR", "Creatinine Clearance (mL/min)")),
								notches=TRUE,
								order=c('Density','QQ','Boxplot'),
								themeTextSize=7.9,
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000'
					)


	print(distMult1)
 
          distMult1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","distMult1")
                           
#Save the Plot
 do.call(pListSave,distMult1)
 FigureTitle <- "Density Plots"
 FigureCaption <- "Caption"
 Footnote <- "Left panel: Frequency histograms of the covariate distributions, where the y-axis shows the probability density of the covariate distribution. The sum of the bar heights times bar widths equal 1. The solid red line is the mean of the covariate distribution. Middle panel: Quantile-quantile plots of the random effect distributions against the standard normal distributions. Solid lines are the reference lines that correspond to perfectly normal distributions.Right panel: Box plot of the covariate distribution, with the grey dash line showing the median of the covariate distribution."
 Type="Figures"

 Plot <- distMult1
 
#Update the Grob
saveGrob(Plot, Name='distMult1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of barchartMult1 ----- 
# 
# 

 plotNum='barchartMult1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=dat,
								dataLimits=list(commands=c("subset( datFile , !duplicated(NMID) )"), within=c(FALSE)))

		barchartMult1=barchartMult(datFile=tempDat,
								catList=list(c("SEX", "Gender"),c("CLCRF", "Creatinine clearance")),
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								nrow=2
					)


	print(barchartMult1)
 
          barchartMult1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","barchartMult1")
                           
#Save the Plot
 do.call(pListSave,barchartMult1)
 FigureTitle <- "Distribution of Subject's Sex, Race, and Disease status"
 FigureCaption <- "Caption"
 Footnote <- ""
 Type="Figures"

 Plot <- barchartMult1
 
#Update the Grob
saveGrob(Plot, Name='barchartMult1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)



# Plotting ----



#Plot of VPC1 ----- 
# 
# 

 plotNum='VPC1'

#Manipulate data by plot specific limitation or transformation
tempDat=manipDat(datFile=list(vpc=vpcDataList[["VPC1"]], addl=vpcDataList[["addlVPC1"]]),
								xBy='TIME',
								markBy='')

		VPC1=VPC(datFile=tempDat,
								xForm=waiver(),
								yForm=waiver(),
								yScale='identity',
								minorTicks=TRUE,
								xBy='TIME',
								fnrow='',
								BQLmethod='Drop',
								simCol='IREP',
								ci=95,
								ciPM=95,
								predCor=TRUE,
								BQLlevel=0.5,
								showObs=TRUE,
								themeAxisTxtColour='#000000',
								themeAxisTitleColour='#000000',
								themePanelBackgroundFill='#FFFFFF',
								themePanelGridColour='#FFFFFF',
								themePlotTitleColour='#000000',
								PI=c(2.5,50,97.5),
								PIns=c(2.5,50,97.5),
								xLab='Time (days)',
								yLab='Concentration (prediction corrected)'
					)


	print(VPC1)
 
          VPC1$fname=file.path("/data/NMStorage/mi210/AMG_NO_2017-01-09/PNG/","VPC1")
                           
#Save the Plot
 do.call(pListSave,VPC1)
 FigureTitle <- "A VPC Plot"
 FigureCaption <- "Observed data (open circles) and median (solid line).  5th and 95th percentiles of the predictions for the 5th, median, and 95th percentiles are shown as shaded regions."
 Footnote <- "Footnote"
 Type="Figures"

 Plot <- VPC1
 
#Update the Grob
saveGrob(Plot, Name='VPC1', file=grobFile, legtit=FigureTitle, leg=FigureCaption, foot=Footnote, type=Type)


#Create an RTF

ordering <- c("NMTab1" , "demogTabCont1" , "demogTabCat1" , "ConcvTime1" , "ConcvTimeGroup1" , "OBSvPRED1" , "paramDist1" , "covCat1" , "covCon1" , "corPairs1" , "QQplot1" , "GOF1" , "distMult1" , "barchartMult1" , "VPC1" , "ConcvTimeMult1")
writeRTF('/data/NMStorage/mi210/AMG_NO_2017-01-09/testing_2017-01-09_Grobs.rda',ordering=ordering)
