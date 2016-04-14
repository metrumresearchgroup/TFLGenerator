debug <- TRUE

#rm(list=ls(all=TRUE))
Sys.setenv(PATH=paste0(Sys.getenv("PATH"),":/usr/bin"))
srcDir <- "/data/tflgenerator"
root <- ifelse(
  dir.exists("/opt/NMStorage_uslv"),
  "/opt/NMStorage_uslv",
  file.path(srcDir,"NMStorage")) # shinyFiles requires starting point for browser, we know this exists.
if(debug){
  debugDir <- file.path(srcDir,"tmp")
  dir.create(debugDir)
}

cat(file=stderr(), "LOG: Start loading packages")
.libPaths("/data/tflgenerator/script/lib")
library(ggplot2,lib="/usr/local/lib/R/site-library")
library(gridExtra,lib="/usr/local/lib/R/site-library")
library(grid)
library(GUI)
library(TFL) # the Amgen Internal TFL package
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(DT)
library(animation)
library(lazyeval)
library(dplyr)
library(gtools)

cat(file=stderr(), "LOG: Finished preamble\n")

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  cat(file=stderr(), "LOG: Entering sourcing of shinyServer function\n")
  
  tryCatch(Defaults<<-DefaultsFirst,
           error=function(e){
             cat(file=stderr(),"LOG: Waiting for DefaultsFirst to load\n")
             cat(file=stderr(),paste("Search path:\n",search()))
             data(DefaultsFirst)
             Defaults <<- DefaultsFirst
           })
  unlockBinding("tabList", as.environment("package:GUI"))
  tabList<<-tabList()
  plotList$sidebarType <- c("Figures","Figures","Tables","Figures","Figures","Figures","Figures",
                            "Figures","Figures","Tables","Listings","Tables","Tables","Tables",
                            "Figures")


  # Get client data
  cdata <- session$clientData
  
  # Values from cdata returned as text
  output$clientDataText <- renderText({
    cat(file=stderr(), "LOG: clientDataText\n")
    cnames <- names(cdata)
    
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep=" = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
    #Open Template
  
  
  observeEvent(input$templateGo,{
    cat(file=stderr(), "LOG: templateGo\n")
    inFile <- input$templatePath
    if (is.null(inFile))
      return(NULL)
    source(inFile$datapath)
  }) 
  
 observe(if(TRUE){  	
  inFile <- input$templatePath
  if (is.null(inFile))
    return(NULL)
  cat(file=stderr(), "LOG: loading template file\n")
  source(inFile$datapath)
 })
  
  ############
  #Setting Color Schemes
  ############
 cat(file=stderr(), "LOG: Setting the color schemes\n")
 unlockBinding("cleanScales", as.environment("package:TFL"))
  
  observe(
    if("Color" %in% names(input)){
      if(input$Color){
        cat(file=stderr(), "LOG: setColorScale\n")
        cleanScales<<-setColorScale()}
    }
  )
  
  observe(
    if("Color" %in% names(input)){
      if(!input$Color){
        cat(file=stderr(), "LOG: setGrayScale\n")
        cleanScales<<-setGrayScale()}
    }
  )
  
  cat(file=stderr(), "LOG: Choosing shinyDir\n")
  shinyDirChoose(input, id="dataPath", session=session, roots=c(NMStorage=root))
  cat(file=stderr(), "LOG: shinyDir chosen\n")
  
  # output$dataPath <- currentWD()
  output$dataPath <- renderText({currentWD()})

  currentWD <- reactive(
    if("dataPath" %nin% names(input)){
      if(("manualDataPath" %in% names(input)) & ("manualDataPath" != Defaults["manualDataPath"])){
          workingDirectory <- input[["manualDataPath"]]
          cat(file=stderr(), "LOG: setting working directory to manualDataPath\n")
          return(workingDirectory)
        }else{
          cat(file=stderr(), "LOG: Using default dataPath\n")
          return(Defaults$dataPath)
        }
    }else{
      # The user elected to use the shinyfiles widget
      cat(file=stderr(), "LOG: using shinyFiles widget for working directory\n")
      workingDirectory <- parseDirPath(roots=c(NMStorage=root),input$dataPath)
      return(workingDirectory)
    }
  )
  
  
  readThis <- reactive({
    list(
      user=Sys.getenv("USER"),
      home=Sys.getenv("HOME"),
      runno=input$runno,
      srcData=input$srcData,
      ext=input$ext,
      currentWD=currentWD(),
      manualDataPath=input$manualDataPath,
      templatePath_name=input$templatePath$name,
      templatePath_datapath=input$templatePath$datapath,
      header=input$header,
      skipLines=input$skipLines,
      dataLimits=input[["dataLimits"]],
      dataTrans=input[["dataTrans"]]
    )
    })
  
  output$readThis <- renderPrint({readThis()})
  
  cat(file=stderr(), "LOG: Entering initial/external interactions\n")

  ##############	
  #Initial/external interactions
  ##############

  # Debugging input
  # load("tmp/shinytmpdat.rda")
  # dataFile <- function() dati
  # load("tmp/tabdat.rda")
  # tableFile <- function() tabdat
  # load("tmp/sourcedat.rda")
  # sourceFile <- function() sourcedat
  # nms <- load("tmp/message.rda")
  # input <- input_vals
  # currentWD <- function() input[["manualDataPath"]]

  #  input <- list()
  #  input$runno <- "0069"
  #  # input$srcData <- "idx_tst_pool_pkf1_cw_mod28_3cwnh_pp_cw50z1b1_3.csv"
  #  # input$srcData <- "0069/fakeSource.csv"
  #  input$ext <- ".tab"
  #  input$header <- TRUE
  #  input$skipLines <- 1
  #  input[["DVCol"]]="DV"
  #  input[["TAFDCol"]]="TIME"
  #  input[["STUDCol"]]="STUD"
  #  input[["NMIDCol"]]="ID"
  #  input[["dataLimits"]] = ""
  #  input[["dataTrans"]] = ""
  
  #read data in a reactive format
  tableFile=reactive({
    cat(file=stderr(), "LOG: tableFile called\n")
    
    if(input$runno=="#"){
      return()
    }
    
    
    if(input$runno!="#"){
      extensions=unlist(str_split(input$ext, ","))
      extensions=gsub("[[:space:]]*", "", extensions)
      
      runs=input$runno
      runs=unlist(str_split(input$runno, ","))
      runs=gsub("[[:space:]]*", "", runs)
      
      
      dat=matrix()
      for(irun in runs) {
        for(iext in extensions){
          ext=iext
          fileName=sprintf("%s/%s/%s%s", currentWD(), irun, irun, ext)
          if(!file.exists(fileName)){
            warning(paste(fileName, 'does not exist'))
            return()
          }			
          foo=read.table(fileName, header=input$header, skip=input$skipLines, stringsAsFactors=F, fill=TRUE)
          foo$Run=irun
          dat=merge(dat, foo, all=TRUE)
          dat=dat[rowSums(is.na(dat)) != ncol(dat),]
          dat$V1=NULL
        }	
      }      
    }
    
    dat=data.frame(dat, stringsAsFactors=F)
    # names(dat)[which(names(dat)==input[["DVCol"]])]="DV"
    # names(dat)[which(names(dat)==input[["TAFDCol"]])]="TAFD"
    # names(dat)[which(names(dat)==input[["NMIDCol"]])]="NMID"
    # names(dat)[which(names(dat)==input[["STUDCol"]])]="STUD"
    # 
    # if("STUD" %in% names(dat)) dat <- dat[order(dat$STUD,dat$NMID,dat$TAFD),] else dat <- dat[order(dat$NMID,dat$TAFD),]
    
    # #rename columns
    # rename=ifelse("renameThese_table" %in% names(input), input[["renameThese_table"]], "")
    # if(nchar(rename)>1){
    #   rename=gsub("\n$", "", rename)
    #   rename=unlist(str_split(rename, "\n"))
    #   rename=str_split(rename, ";[[:space:]]*")
    #   for(i in length(rename)){
    #     if(nchar(rename[[i]][1])>1){
    #       names(dat)[which(names(dat)==rename[[i]][1])]=rename[[i]][2]
    #     }	
    #   }
    # }
    # 
    # # #factor entire sets
    # fF=ifelse("factorThese_table" %in% names(input), input[["factorThese_table"]], "")
    # if(nchar(fF)>1){
    #   fF=gsub("\n$", "", fF)
    #   fF=unlist(str_split(fF, "\n"))
    #   fF=str_split(fF, ";[[:space:]]*")
    #   for(i in length(fF)){
    #     if(nchar(fF[[i]][1])>1){
    #       dat[,fF[[i]][1]]=factor(dat[,fF[[i]][1]], levels=unlist(str_split(fF[[i]][2], ",[[:space:]]*")), labels=unlist(str_split(fF[[i]][3], ",[[:space:]]*")))
    #     }
    #   }
    # }
    # 
    # # #perform limits and transformations
    # # #Deal with formatting the data limits for functions
    # wholeLim=limitations(input[["dataLimits_table"]])
    # wholeTrans=transformations(input[["dataTrans_table"]])
    # dat=manipDat(dat, dataLimits=wholeLim, dataTrans=wholeTrans)
    
    parsecommands <- cleanparse(input[["dataParse_table"]],"dat")
    if(!is.null(parsecommands)){
      for(i in 1:length(parsecommands$commands)){
        if(parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- within(dat, {", parsecommands$commands[i], "})"))),
                                             error=function(e) cat(file=stderr(),paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e)))
        if(!parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- ", parsecommands$commands[i]))),
                                              error=function(e) cat(file=stderr(),print(paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e))))
      }
    }
    
    # For debugging, save a copy of input
    if(debug){
      tabdat <- dat
      tabList <- get("tabList",envir=.GlobalEnv)
      save(tabdat,file=file.path(debugDir,"tabdat.rda"))
    }
    # End debugging
    cat(file=stderr(), "LOG: tableFile finished successfully\n")
    return(dat)
  })	
  
  cat(file=stderr(), "LOG: End dataFile definition\n")
  
  ##################################################################################################
  #read data in a reactive format
  sourceFile=reactive({
    cat(file=stderr(), "LOG: sourceFile called\n")
    
    if(input$srcData=="sourcedata.csv"){
      return()
    }
    
    if(input$srcData %nin% c("", " ", "sourcedata.csv")){
      srcDatFile=sprintf("%s/%s", currentWD(), input$srcData)
      if(!file.exists(srcDatFile)){
        warning(paste(srcDatFile, 'does not exist'))
        return()
      }			
      foo=as.best(read.csv(srcDatFile, stringsAsFactors=F, fill=TRUE))
      if(exists("dat")) dat=merge(foo, dat, all=TRUE) else dat <- foo
      dat=dat[rowSums(is.na(dat)) != ncol(dat),]
    }
    
    dat=data.frame(dat, stringsAsFactors=F)
    # names(dat)[which(names(dat)==input[["DVCol"]])]="DV"
    # names(dat)[which(names(dat)==input[["TAFDCol"]])]="TAFD"
    # names(dat)[which(names(dat)==input[["NMIDCol"]])]="NMID"
    # names(dat)[which(names(dat)==input[["STUDCol"]])]="STUD"
    # 
    # if("STUD" %in% names(dat)) dat <- dat[order(dat$STUD,dat$NMID,dat$TAFD),] else dat <- dat[order(dat$NMID,dat$TAFD),]
    
    #rename columns
    # rename=ifelse("renameThese" %in% names(input), input[["renameThese"]], "")
    # if(nchar(rename)>1){
    #   rename=gsub("\n$", "", rename)
    #   rename=unlist(str_split(rename, "\n"))
    #   rename=str_split(rename, ";[[:space:]]*")
    #   for(i in length(rename)){
    #     if(nchar(rename[[i]][1])>1){
    #       names(dat)[which(names(dat)==rename[[i]][1])]=rename[[i]][2]
    #     }	
    #   }
    # }
    # 
    # # #factor entire sets
    # fF=ifelse("factorThese_source" %in% names(input), input[["factorThese_source"]], "")
    # if(nchar(fF)>1){
    #   fF=gsub("\n$", "", fF)
    #   fF=unlist(str_split(fF, "\n"))
    #   fF=str_split(fF, ";[[:space:]]*")
    #   for(i in length(fF)){
    #     if(nchar(fF[[i]][1])>1){
    #       dat[,fF[[i]][1]]=factor(dat[,fF[[i]][1]], levels=unlist(str_split(fF[[i]][2], ",[[:space:]]*")), labels=unlist(str_split(fF[[i]][3], ",[[:space:]]*")))
    #     }
    #   }
    # }
    # 
    # #perform limits and transformations
    # # #Deal with formatting the data limits for functions
    # wholeLim=limitations(input[["dataLimits_source"]])
    # wholeTrans=transformations(input[["dataTrans_source"]])
    # dat=manipDat(dat, dataLimits=wholeLim, dataTrans=wholeTrans)
    parsecommands <- cleanparse(input[["dataParse_source"]],"dat")
    if(!is.null(parsecommands)){
      for(i in 1:length(parsecommands$commands)){
        if(parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- within(dat, {", parsecommands$commands[i], "})"))),
                                             error=function(e) cat(file=stderr(),paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e)))
        if(!parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- ", parsecommands$commands[i]))),
                                              error=function(e) cat(file=stderr(),print(paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e))))
      }
    }
    
    # For debugging, save a copy of input
    if(debug){
      sourcedat <- dat
      tabList <- get("tabList",envir=.GlobalEnv)
      save(sourcedat,file=file.path(debugDir,"sourcedat.rda"))
    }
    # End debugging
    cat(file=stderr(), "LOG: sourceFile finished successfully\n")
    return(dat)
  })	
  
  cat(file=stderr(), "LOG: End sourceFile definition\n")
  
  #############################################################################
  
  #read data in a reactive format
  dataFile=reactive({
    cat(file=stderr(), "LOG: dataFile called\n")
    
    if(is.null(tableFile()) & is.null(sourceFile())){
      return()
    }
    
    if(!is.null(tableFile()) & !is.null(sourceFile())){
      dat <- merge(sourceFile(), tableFile(), all=TRUE) 
    }else {
      if(!is.null(tableFile())) dat <- tableFile() else dat <- sourceFile()
    }
    

    # dat=data.frame(dat, stringsAsFactors=TRUE)
    if("DVCol" %in% names(input)) names(dat)[which(names(dat)==input[["DVCol"]])] = "DV"
    if("TAFDCol" %in% names(input)) names(dat)[which(names(dat)==input[["TAFDCol"]])]="TAFD"
    if("NMIDCol" %in% names(input)) names(dat)[which(names(dat)==input[["NMIDCol"]])]="NMID"
    if("STUDCol" %in% names(input)) names(dat)[which(names(dat)==input[["STUDCol"]])]="STUD"
    if("IPREDCol" %in% names(input)) names(dat)[which(names(dat)==input[["IPREDCol"]])]="IPRED"
    if("PREDCol" %in% names(input)) names(dat)[which(names(dat)==input[["PREDCol"]])]="PRED"
    
    if(all(c("DV","TAFD","NMID")%in%names(dat))){
      if("STUD" %in% names(dat)) dat <- dat[order(dat$STUD,dat$NMID,dat$TAFD),] else dat <- dat[order(dat$NMID,dat$TAFD),]
    }
    
    # #rename columns
    # rename=ifelse("renameThese" %in% names(input), input[["renameThese"]], "")
    # if(nchar(rename)>1){
    #   rename=gsub("\n$", "", rename)
    #   rename=unlist(str_split(rename, "\n"))
    #   rename=str_split(rename, ";[[:space:]]*")
    #   for(i in length(rename)){
    #     if(nchar(rename[[i]][1])>1){
    #       names(dat)[which(names(dat)==rename[[i]][1])]=rename[[i]][2]
    #     }	
    #   }
    # }
  
    #factor entire sets
    # fF=ifelse("factorThese" %in% names(input), input[["factorThese"]], "")
    # if(nchar(fF)>1){
    #   fF=gsub("\n$", "", fF)
    #   fF=unlist(str_split(fF, "\n"))
    #   fF=str_split(fF, ";[[:space:]]*")
    #   for(i in length(fF)){
    #     if(nchar(fF[[i]][1])>1){
    #       dat[,fF[[i]][1]]=factor(dat[,fF[[i]][1]], levels=unlist(str_split(fF[[i]][2], ",[[:space:]]*")), labels=unlist(str_split(fF[[i]][3], ",[[:space:]]*")))	
    #     }	
    #   }
    # }
    # 
    # #perform limits and transformations
    # #Deal with formatting the data limits for functions
    # wholeLim=limitations(input[["dataLimits"]])	
    # wholeTrans=transformations(input[["dataTrans"]])
    # dat=manipDat(dat, dataLimits=wholeLim, dataTrans=wholeTrans)
    parsecommands <- cleanparse(input[["dataParse"]],"dat")
    if(!is.null(parsecommands)){
      for(i in 1:length(parsecommands$commands)){
        if(parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- within(dat, {", parsecommands$commands[i], "})"))),
                                             error=function(e) cat(file=stderr(),paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e)))
        if(!parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- ", parsecommands$commands[i]))), 
                                              error=function(e) cat(file=stderr(),print(paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e))))
      }
    }
    
    # For debugging, save a copy of input
    if(debug){
      dati <- dat
      tabList <- get("tabList",envir=.GlobalEnv)
      save(dati,file=file.path(debugDir,"shinytmpdat.rda"))
    }
    # End debugging
    cat(file=stderr(), "LOG: dataFile finished successfully\n")
    return(dat)
  })	
  
  cat(file=stderr(), "LOG: End dataFile definition\n")
  
  
  ##############	
  # Output Renders - Just the data set overview and plot title
  ##############
 

   
  #Raw Contents
  output$contentsHead_tabledata <- DT::renderDataTable({
    cat(file=stderr(), "LOG: contentsHead_tabledata called\n")
    if("runno" %nin% names(input)){
        return()
      }
      if(is.null(tableFile())){
        return()
      }
    return(datatable(tableFile(),filter="top"))
  })
  output$contentsHead_sourcedata <- DT::renderDataTable({
    cat(file=stderr(), "LOG: contentsHead_sourcedata called\n")
    if("srcData" %nin% names(input)){
      return()
    }
    if(is.null(sourceFile())){
      return()
    }
    return(datatable(sourceFile(), filter="top"))
  })  
  output$contentsHead_analysisdata <- DT::renderDataTable({
    cat(file=stderr(), "LOG: contentsHead_analysisdata called\n")
    if("srcData" %nin% names(input) & "runno" %nin% names(input)){
      return()
    }
    if(is.null(dataFile())){
      return()
    }
    return(datatable(dataFile(), filter="top"))
  })  
  
  #Raw Contents
  summarizeContents <- function(data){
    cat(file=stderr(), "LOG: performing contents summary\n")
    if(is.null(data)){
      return()
    }
        
    fakeData=data
    classes <- sapply(fakeData,class)
    fakeData <- summary(fakeData)
    dimnames(fakeData)[[2]] <- sprintf("%s (%s)",str_trim(dimnames(fakeData)[[2]]),classes)
    return(fakeData)
  }
  output$contentsSummary_tabledata <- renderPrint({summarizeContents(tableFile())})
  output$contentsSummary_sourcedata <- renderPrint({summarizeContents(sourceFile())})
  output$contentsSummary_analysisdata <- renderPrint({summarizeContents(dataFile())})
  

  output$contentsStat <- DT::renderDataTable({
    cat(file=stderr(), "LOG: calculating contents statistics\n")
    if("runno" %in% names(input)){	
      if (input$runno=="#" & input$srcData=="sourcedata.csv") {
        return()
      }
      if(is.null(dataFile())){
        return()
      }
      
      fakeData=apply(dataFile(), c(2), unique)
      #	fakeData=sapply(fakeData, as.numeric)
      fakeData=sapply(fakeData, function(x){
        if(length(x)>15){
          x=c(min(x,na.rm=TRUE), max(x, na.rm=TRUE), sample(x, 13, replace=FALSE))
          x=x[order(x)]
        }
        if(length(x)<=15){
          x=unique(x)
          if(length(x)<15){
            x=c(x[order(x)], rep("", times=(15-length(x))))
          }	
        }
        return(x)})
      fakeData=data.frame(fakeData, check.names=FALSE, stringsAsFactors=FALSE)
      fakeData=fakeData[,names(dataFile())]
      return(fakeData)
    }
  })
  
  
  output$projectTitle <- renderText({
    cat(file=stderr(), "LOG: printing project title\n")
    #This prints out the input project title to the appropriate GUI location
    input$projectTitle
  })
  
  cat(file=stderr(), "LOG: End data summary tab definitions\n")
#Data Input Tabset
  
  output$DataTabset <- renderUI({
    cat(file=stderr(), "LOG: creating data input tabset\n")
    # choices <- names(dataFile())
    # if(is.null(choices)) choices <- ""
    
    tabsetPanel(
             tabPanel(title="Project Information",
                      wellPanel(
                        textInput(inputId="projectTitle", label="Project Title:", value=Defaults$projectTitle),
                        boxInputLarge(inputId="projectInfo", label="Project Information:", value=Defaults$projectInfo)
                        )
                      ),
             tabPanel(title="Model Info",
                      wellPanel(
                        textInput(inputId="manualDataPath", label="Parent working directory:", value=""),	
                        textInput(inputId="srcData", label='NONMEM source data:',value=Defaults$srcData),
                        textInput(inputId="runno", label="Run Number:", value=Defaults$runno),
                        #textInput(inputId="numModel", label="Number of Models", value="1"),
                        textInput(inputId="ext", label="File Extensions:", value=Defaults$ext),
                        checkboxInput('header', 'Header?', value=Defaults$header),
                        numericInput("skipLines", "Skip Lines:", value=Defaults$skipLines)
                        #,
                        #textInput(inputId="baseModel", label="Base Model #", value="")
                      )
              ),
             tabPanel(title="Modify Data",
                    fluidPage(
                      wellPanel(
                        fluidRow( 
                          column( 12,
                                  #h3("Table data:"),
                                  # boxInputLarge(inputId="renameThese_table", "Rename Columns", value=Defaults$renameThese),
                                  # boxInputLarge(inputId="factorThese_table", "Factor Columns", value=Defaults$factorThese),
                                  # boxInputLarge(inputId="dataTrans_table", "Transform Data:", value=Defaults$dataTrans),
                                  # boxInputLarge(inputId="dataLimits_table", "Limit Data by", value=Defaults$dataLimits),
                                  boxInputLarge(inputId="dataParse_table", "Table data manipulation code:", value=Defaults$dataParse_table),

                                  h1(""),
                                  
                                  #h3("Source data:"),
                                  # boxInputLarge(inputId="renameThese_source", "Rename Columns", value=Defaults$renameThese),
                                  # boxInputLarge(inputId="factorThese_source", "Factor Columns", value=Defaults$factorThese),
                                  # boxInputLarge(inputId="dataLimits_source", "Limit Data by", value=Defaults$dataLimits),
                                  # boxInputLarge(inputId="dataTrans_source", "Transform Data:", value=Defaults$dataTrans),
                                  boxInputLarge(inputId="dataParse_source", "Source data manipulation code:", value=Defaults$dataParse_source),

                                  h1(""),
                                  #h3("Combined data:"),
                                  # boxInputLarge(inputId="renameThese", "Rename Columns", value=Defaults$renameThese),
                                  # boxInputLarge(inputId="factorThese", "Factor Columns", value=Defaults$factorThese),
                                  # boxInputLarge(inputId="dataLimits", "Limit Data by", value=Defaults$dataLimits),
                                  # boxInputLarge(inputId="dataTrans", "Transform Data:", value=Defaults$dataTrans),
                                  boxInputLarge(inputId="dataParse", "Analysis data manipulation code:", value=Defaults$dataParse_analysis)
                          )
                        )
                      )  
                    )
             ),
             tabPanel(title="Change E-R SSAP Defaults",
                      wellPanel( 
                        textInput("DVCol", "DV Column", Defaults$DVCol),
                        textInput("TAFDCol", "TAFD Column", Defaults$TAFDCol),
                        textInput("STUDCol", "STUD Column", Defaults$STUDCol),
                        textInput("NMIDCol", "NMID Column", Defaults$NMIDCol),
                        textInput("IPREDCol", "IPRED Column", Defaults$IPREDCol),
                        textInput("PREDCol", "PRED Columns", Defaults$PREDCol)
                        # selectInput("DVCol", "DV Column", choices=choices, selected=Defaults$DVCol, selectize=T),
                        # selectInput("TAFDCol", "TAFD Column", choices=choices, selected=Defaults$TAFDCol, selectize=T),
                        # selectInput("STUDCol", "STUD Column", choices=choices, selected=Defaults$STUDCol, selectize=T),
                        # selectInput("NMIDCol", "NMID Column", choices=choices, selected=Defaults$NMIDCol, selectize=T)
                        
                      )
             )
    )
  })

#Data Tabset  
  output$outputTabset <- renderUI({		
    cat(file=stderr(), "LOG: creating data view tabset \n")
    #The first PanelSet is what is loaded with the base defaults.  
    PanelSet=list(
      tabPanel("Run Data",
               fluidRow(
                 column(width = 12,
                        box(
                          title = "", width = NULL, status = "primary",
                          div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsHead_tabledata'))
                        )
                 )
               )
      ),
      tabPanel("Run Data Summary", verbatimTextOutput("contentsSummary_tabledata")),
      tabPanel("Source Data",
               fluidRow(
                 column(width = 12,
                        box(
                          title = "", width = NULL, status = "primary",
                          div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsHead_sourcedata'))
                        )
                 )
               )
      ),
      tabPanel("Source Data Summary", verbatimTextOutput("contentsSummary_sourcedata")),
      tabPanel("Analysis Data",
               fluidRow(
                 column(width = 12,
                        box(
                          title = "", width = NULL, status = "primary",
                          div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsHead_analysisdata'))
                        )
                 )
               )
      ),
      tabPanel("Analysis Data Summary", verbatimTextOutput("contentsSummary_analysisdata"))

                            # fluidRow(
                            #   column(width = 12,
                            #          box(
                            #            title = "Summary", width = NULL, status = "primary",
                            #            div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsSummary'))
                            #          )
                            #   )
                            # )
                            # ,
                            # fluidRow(
                            #   column(width = 12,
                            #          box(
                            #            title = "Statistics", width = NULL, status = "primary",
                            #            div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsStat'))
                            #          )
                            #   )
                            # )
                  # 
                  #                             )
    )
    dummy=(do.call(tabsetPanel, PanelSet))
    return(dummy)
    

  })
  

# Analysis Selection
  output$PlotTabset<-renderUI({
    cat(file=stderr(), "LOG: creating analysis selection tabset\n")
    panelList=list()
    j=1
    for(item in unique(tabList$tabType)){
      testList=list()
      for(i in c(1:length(tabList$label[which(tabList$tabType==item)]))){
        
        testList[[i]]=do.call(what=textInput, args=list(inputId=tabList$inputId[which(tabList$tabType==item)][[i]],
                                                      label=tabList$label[which(tabList$tabType==item)][[i]],
                                                      value=Defaults[[tabList$inputId[which(tabList$tabType==item)][[i]]]])
        )
      }
      wellList=do.call(what=wellPanel, args=testList)
      panelList[[j]]=(do.call(what=tabPanel, args=list(title=item, wellList)))
      j=j+1
    }
    stuff=do.call(what=tabsetPanel, panelList)

    return(do.call(what=tabPanel, args=list("Figures", stuff)) )
  })

#Figures, Tables and Listings Tabset
  output$figuresTabset<-renderUI({
    cat(file=stderr(), "LOG: creating figures tabset \n")
    type="Figures"
    types=grep(type, plotList$sidebarType)
    PanelSet=list()
    for (item in plotList$type[types]){
      if(paste(item, "Num", sep="") %in% names(input)){
        if("varNames" %in% names(formals(plotList$Call[plotList$type==item]))){
          PanelSet=do.call(what=plotList$Call[plotList$type==item], args=list(plotType=item, input=input, Set=PanelSet, varNames=names(dataFile())) )
        }else{
          PanelSet=do.call(what=plotList$Call[plotList$type==item], args=list(plotType=item, input=input, Set=PanelSet ))
        }
      }
    }
    return(do.call(tabsetPanel, PanelSet))

  })
  
  output$listingsTabset<-renderUI({
    cat(file=stderr(), "LOG: creating listings tabset\n")
    type="Listings"
    types=grep(type, plotList$sidebarType)
    PanelSet=list()
    for (item in plotList$type[types]){
      if(paste(item, "Num", sep="") %in% names(input)){
        PanelSet=do.call(what=plotList$Call[plotList$type==item], args=list(plotType=item, input=input, Set=PanelSet) )
      }
    }
    return(do.call(tabsetPanel, PanelSet))
    
  })
  output$tablesTabset<-renderUI({
    cat(file=stderr(), "LOG: creating tables tabset\n")
    type="Tables"
    types=grep(type, plotList$sidebarType)
    PanelSet=list()
    for (item in plotList$type[types]){
      if(paste(item, "Num", sep="") %in% names(input)){
        PanelSet=do.call(what=plotList$Call[plotList$type==item], args=list(plotType=item, input=input, Set=PanelSet) )
      }
    }
    return(do.call(tabsetPanel, PanelSet))
    
  })

  output$currentTFLTabset <- renderUI({
  cat(file=stderr(), "LOG: creating current TFL tabset\n")
    outList <- data.frame()
    for(item in plotList$type){
      numbers=as.numeric(unlist(str_extract_all(input[[paste(item, "Num", sep="")]], "\\d+")))
      if(length(numbers)>1){numRange=numbers[which(numbers!=0)]}
      if(length(numbers)==1){numRange=c(0:numbers)}
      if(length(numbers)==0){numRange=0}
      numRange <- setdiff(numRange,0)
      for(n in numRange){
        outList <- rbind(outList,data.frame(object=paste0(item,numRange), 
                                            title=input[[paste0("LegendTitle",item,numRange)]],
                                            type=plotList$sidebarType[plotList$type==item]))
      }
    }
    outList$type <- factor(outList$type,c("Tables","Figures","Listings"))
    outList <- dlply(outList,.(type))
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "figureOrder", label="Figures order", 
          choices=as.character(outList$Figures$title), 
          multiple=T, options=list(create=F)
        ),
        selectizeInput(
          "tableOrder", label="Tables order", 
          choices=as.character(outList$Tables$title), 
          multiple=T, options=list(create=F)
        ),
        selectizeInput(
          "listingOrder", label="Listings order", 
          choices=as.character(outList$Listings$title), 
          multiple=T, options=list(create=F)
        )
      ),
      mainPanel(
        h2("Tables:"),
        verbatimTextOutput("tflOrder_tables"),
        h1(""),
        h2("Figures:"),
        verbatimTextOutput("tflOrder_figures"),
        h1(""),
        h2("Listings:"),
        verbatimTextOutput("tflOrder_listings")
      )
    )
    
  })
  
  output$tflOrder_tables <- renderPrint({
    cat(file=stderr(), "LOG: matching table order to that specified\n")
    tryCatch(
      data.frame(Label=paste0("Table ", 1:length(input[["tableOrder"]])),
                 Title=input[["tableOrder"]]),
      error=function(e) print("Unspecified")
    )
  })
  
  output$tflOrder_figures <- renderPrint({
    cat(file=stderr(), "LOG: matching figure order to that specified\n")
    tryCatch(
        data.frame(Label=paste0("Figure ", 1:length(input[["figureOrder"]])),
                 Title=input[["figureOrder"]]),
        error=function(e) print("Unspecified")
    )
  })
  output$tflOrder_listings <- renderPrint({
    cat(file=stderr(), "LOG: matching listing order to that specified\n")
    tryCatch(
        data.frame(Label=paste0("Listing ", 1:length(input[["listingOrder"]])),
                 Title=input[["listingOrder"]]),
        error=function(e) print("Unspecified")
    )
  })
  
  ############
  #Generating Plots, internal and saving
  ############

  cat(file=stderr(), "LOG: Begin generating plots\n")
  for (this_item in plotList$type){
   
    local({
      item=this_item
      #Observe if any plots of that type have been assigned inputs
      observe(if(length(grep(paste("reset", item, sep=""), names(input)))>0){
        
        numbers=as.numeric(unlist(str_extract_all(input[[paste(item, "Num", sep="")]], "\\d+")))
        if(length(numbers)>1){numRange=numbers[which(numbers!=0)]}
        if(length(numbers)==1){numRange=c(0:numbers)}
        if(length(numbers)==0){numRange=0}
        
        for (this_n in numRange){
          local({
            n=this_n
            if(n!=0){	
              observeEvent(input[[paste("button",item,n,sep="")]],{ 
                if(debug){
                  message <- "DEBUG A"
                  input_nms <- names(input)
                  input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                  names(input_vals) <- input_nms
                  save(message, input_vals,n,file=file.path(debugDir,"message.rda"))
                } 
                #check if the defaults/inputs for a plot have been created
                if(length(grep(paste(item, n,sep=""), names(input)))>0){
                 
                  cat(file=stderr(), paste("LOG: checking priors for", item,n, "\n"))
                  
                  if(debug){
                    message <- "DEBUG AA"
                    save(message,file=file.path(debugDir,"message.rda"))
                  }                     
                  
                  ##Check to see if the input has changed since the last time it was rendered
                  idx=grep(paste(item,n,sep=""), names(Defaults), value=TRUE)
                  idn=grep(paste(item,n,sep=""), names(input), value=TRUE)
                  
                  if(debug){
                    message <- "DEBUG AAA"
                    save(message,idx,idn,Defaults,file=file.path(debugDir,"message.rda"))
                  }                     
          
                  
                  #some values don't exist as inputs, for example the priors variable to check for priors
                  idtest=idx[idx %in% idn]
                  
                  
                  #because of the reactive nature of 'input' comparisons have to be done one at a time
                  if(length(idtest)>0){
                    sameAsDefault=sum(sapply(idtest, function(X){all(input[[X]]==Defaults[X])}))/length(idtest)
                  }
                  
                  if(debug){
                    message <- "DEBUG AAAA"
                    input_nms <- names(input)
                    input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                    names(input_vals) <- input_nms
                    
                    save(message,idx,idn,input_vals,item,n,Defaults,sameAsDefault,
                         file=file.path(srcDir,"tmp","message.rda"))
                  }
                  
                  if(sameAsDefault!=1){
                    cat(file=stderr(), paste("LOG: creating", item, n, "\n"))
                    argList=try(createArgList(input, item, n, dataFile=dataFile(), currentWD=currentWD()))
                    
                    # Special routine for multipage
                    if(item == "ConcvTimeMult"){
                      idtest=idx[idx %in% idn]
                      idtest <- setdiff(idtest,paste0("page",item,n))
                      sameAsDefault <- sum(sapply(idtest, function(X){input[[X]]==Defaults[X]}))/length(idtest)
                      # If page is the only thing that changed, don't regen all plots
                      if(sameAsDefault==1) argList$regenPlots <- F else argList$regenPlots <- T
                      # if(debug){
                      #   message="ConcvTime sameAsDefault"
                      #   input_nms <- names(input)
                      #   input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                      #   names(input_vals) <- input_nms
                      #   save(message,idx,idn,Defaults,argList,input_vals,file=file.path(debugDir,"page.rda"))
                      # }
                    }
                    
                    if(debug){
                      message <- "DEBUG B"
                      input_nms <- names(input)
                      input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                      names(input_vals) <- input_nms
                      
                      save(message,argList, input_vals, file=file.path(debugDir,"message.rda"))
                    } 
                    
                    
                    callType=argList$callType
                    argList$callType=NULL
                    
                    
                    if(is.null(dataFile())){
                      return()
                    }
                    
                    if(debug){
                      message <- "DEBUG B"
                      input_nms <- names(input)
                      input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                      names(input_vals) <- input_nms
                      
                      save(message,argList, input_vals, idx, Defaults, file=file.path(debugDir,"setdefaults.rda"))
                    } 
                    
                    if(length(idx)>0){
                      for(IDX in idx){
                        Defaults[[IDX]]<<-input[[IDX]]
                      }
                    }
                    Defaults[[paste("priorExists", item, n, sep="")]]<<-TRUE

                    if(debug){
                      message <- "DEBUG C"
                      save(message,file=file.path(debugDir,"message.rda"))
                    } 
                  
                                      
                    #insert an error block around the plotting
                    p1 = tryCatch({
                      if(debug) save(callType,argList,file=file.path(debugDir,"output.rda"))
                      do.call(callType,args=argList)
                    }, 
                    # warning = function(w) {
                    #   arrangeGrob(textGrob(sprintf("You broke something\n%s", w)),
                    #               do.call(callType,args=argList),
                    #               heights=c(0.05,1))
                    # }, 
                    error = function(e) {
                      if(debug) save(callType,argList,e,file=file.path(debugDir,"error.rda"))
                      arrangeGrob(textGrob(sprintf("You broke something\n%s", e)))
                    }
                    )
                    
                    if(debug){
                      message <- "DEBUG D"
                      save(message,file=file.path(debugDir,"message.rda"))
                    } 
                    
                    
                    if(item %nin% c("demogTabCont","demogTabCat","NMTab","ConcvTimeMult")){
                      #Perform the actual plotting
                      output[[paste("Plot", item,n, sep="")]]<<-renderPlot({
                        print(p1)
                      })
                    }else if(item=="ConcvTimeMult"){
                      output[[paste("Plot",item,n,sep="")]] <<- renderImage({ p1 },deleteFile=F)
                    }else{
                      output[[paste("Plot",item,n,sep="")]]<<-renderImage(
                        renderTex(obj=p1,item=paste0(item,n),
                                  margin=c(left=input[[paste0("leftmargin",item,n)]],
                                           top=input[[paste0("topmargin",item,n)]],
                                           bottom=input[[paste0("bottommargin",item,n)]],
                                           right=input[[paste0("rightmargin",item,n)]]))
                        ,deleteFile=F)
                    }
                    
                    if(debug){
                      input_nms <- names(input)
                      input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                      names(input_vals) <- input_nms
                      message <- "DEBUG E"
                      save(message,argList,input_vals,item,n,p1,file=file.path(srcDir,"tmp","message.rda"))
                    } 
                    
                    
                  } # End sameAsDefault!=1
                  
                  observeEvent(input$outputGo,{
                    if(input$saveAs!=""){
                      cat(file=stderr(), paste("LOG: outputGo for", item, n, 
                                               ", outputGo value:", input$outputGo, 
                                               ", inputButton value:", input[[paste("button",item,n,sep="")]],"\n"))
                      if(debug){
                        message <- "DEBUG OUTPUTGO"
                        input_nms <- names(input)
                        input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                        names(input_vals) <- input_nms
                        
                        save(message,item,n,input_vals,idx,Defaults,file=file.path(debugDir,"message.rda"))
                      }
                      argList=createArgList(input, item, n, dataFile=dataFile(), currentWD=currentWD())
                      callType=argList$callType
                      argList$callType=NULL
                      
                      
                      if(is.null(dataFile())){
                        return()
                      }
                      
                      for(IDX in idx){
                        Defaults[[IDX]]<<-input[[IDX]]
                      }
                      Defaults[[paste("priorExists", item, n, sep="")]]<<-TRUE

                      if(item%in%"ConcvTimeMult") argList$tmpDir <- file.path(Dir,"PNG")
                      
                      #insert an error block around the plotting
                      p1 = tryCatch({
                        if(debug) save(callType,argList,file=file.path(debugDir,"output.rda"))
                        do.call(callType,args=argList)
                      }, 
                      # warning = function(w) {
                      #   arrangeGrob(textGrob(sprintf("You broke something\n%s", w)),
                      #               do.call(callType,args=argList),
                      #               heights=c(0.05,1))
                      # }, 
                      error = function(e) {
                        if(debug) save(callType,argList,file=file.path(debugDir,"error.rda"))
                        arrangeGrob(textGrob(sprintf("You broke something\n%s", e)))
                      })
                      
                      
                      if(item %nin% c("demogTabCont","demogTabCat","NMTab")){
                        #Perform the actual plotting
                        output[[paste("Plot", item,n, sep="")]]<<-renderPlot({
                          print(p1)
                        })					
                      }else if(item=="ConcvTimeMult"){
                        output[[paste("Plot",item,n,sep="")]] <<- renderImage({ p1 },deleteFile=F)
                      }else{
                        output[[paste("Plot",item,n,sep="")]]<<-renderImage(
                          renderTex(obj=p1,item=paste0(item,n),
                                    margin=c(left=input[[paste0("leftmargin",item,n)]],
                                             top=input[[paste0("topmargin",item,n)]],
                                             bottom=input[[paste0("bottommargin",item,n)]],
                                             right=input[[paste0("rightmargin",item,n)]]))
                          ,deleteFile=F)
                      }
                      
                      
                      #Create the save directory
                      
                      Dir=sprintf("%s/%s_%s/", currentWD(), 
                                  gsub("'","",
                                       gsub("[[:space:]]|\\.", "_", input$projectTitle)
                                  ), 
                                  Sys.Date()
                      )
                      
                      
                      fileHead=sprintf("%s%s_%s",Dir, input$saveAs, Sys.Date())
                      # cat(Dir)
                      
                      if(debug){
                        message="DEBUG G"
                        input_nms <- names(input)
                        input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                        names(input_vals) <- input_nms
                        save(message,input_vals,item,Dir,fileHead,callType, argList, file=file.path(debugDir,"message.rda"))
                      }  
                      
                      dir.create(Dir,showWarning=FALSE)
                      ###############
                      #			Save plots and grobs, record the script
                      ################			
                      
                      
                      p1csv=data.frame(1) # What is this?
                      
                      #What to do with TeX tables?
                      # if(callType %in% c("demogTabCont","demogTabCat","NMTab")){
                      #   # p1csv=p1
                      #   # p1=renderTex(obj=p1,item,tmpDir=Dir)
                      # }

                      
                      p1List=list(Facets=ifelse("facetBy" %in% names(argList), argList$facetBy, ""),
                                  Marks=ifelse("markBy" %in% names(argList), argList$markBy, ""),
                                  Groups=ifelse("groupBy" %in% names(argList), argList$groupBy, ""),
                                  Stratification="",
                                  LegendTitle=ifelse(paste("LegendTitle", item, n, sep="") %in% names(input), input[[paste("LegendTitle", item, n, sep="")]], ""),
                                  Legend=ifelse(paste("Legend", item, n, sep="") %in% names(input), input[[paste("Legend", item, n, sep="")]], ""),
                                  Plot=p1,
                                  Type=plotList$sidebarType[plotList$type==item],
                                  CSV=p1csv
                      )
                      p1Name=paste(item,n, sep="")
                      if(input$PNG){
                        if(callType %nin% c("demogTabCont","demogTabCat","RNM","ConcvTimeMult")){
                          savePlots(plotName=p1,  directory=Dir, saveName=paste(item,n, sep=""))
                        }else if(item=="ConcvTimeMult"){
                          p1List$Plot <- p1['src']
                        }else{
                          dir.create(file.path(Dir,"PNG"),showWarnings = F)
                          f <- renderTex(p1,item,n,p1List$Footnote,tmpDir=file.path(Dir,"PNG"),margin=c(left=10,top=5,right=50,bottom=5))
                          p1List$Plot <- f['src']
                        }
                      }
                      # This is massively inefficient!
                      saveGrob(plot=p1List, Name=p1Name, file=sprintf("%s_Grobs.rda", fileHead))
                      
                      #Insert function recording into script	
                      #Save only the non-default arguments unless the user asks for a verbose script
                      
                      useArgs=argList
                      if(input$verbose){useArgs=createArgList(input,item,n,dataFile(),currentWD=currentWD(),complete=T)}
                      
                      #reduce argList to arguments used in the dataManip
                      argListManip=argList[names(argList) %in% names(formals(manipDat))]
                      
                      #reduce argList to non-default arguments 
                      for(this_name in names(argListManip)){
                        if(this_name!="datFile"){
                          if(is.null(argListManip[[this_name]]) & is.null(formals(manipDat)[[this_name]])){
                            argListManip=argListManip[names(argListManip)[names(argListManip)!=this_name]]
                          }
                          if(!is.null(argListManip[[this_name]]) & !is.null(formals(manipDat)[[this_name]]) ){
                            if(all(argListManip[[this_name]]==formals(manipDat)[[this_name]])){	
                              argListManip=argListManip[names(argListManip)[names(argListManip)!=this_name]]		
                            }
                          }	
                        }
                      }
                      
                      # #Keep a recording of the complete list <-this is only used to write out the complete argument list in the verbose script
                      # argListComplete=formals(callType)
                      # argListComplete$"..."=NULL
                      # for(listName in names(argList)){
                      #   argListComplete[[listName]]=argList[[listName]]
                      # }
                      if(debug){
                        input_vals <- reactiveValuesToList(input,all.names=T)
                        save(callType, useArgs, input_vals, n, argListManip, file=file.path(srcDir,"tmp","message.rda"))
                      }
                      tryCatch(
                        recordGUI(doWhat=callType, 
                                toWhat=useArgs,
                                input=input,
                                number=n,
                                manipArgs=argListManip,
                                currentWD=currentWD()),
                        error=function(e){
                          cat(file=stderr(), sprintf("Record GUI failed with %s %s\n",item,n))
                          if(debug){
                          input_vals <- reactiveValuesToList(input,all.names=T)
                          save(e,callType, useArgs, input_vals, n, argListManip, file=file.path(srcDir,"tmp","record_output.rda"))
                          }
                          
                        }
                      )
                      
                      
                    }	
                  })   # End "OutputGo"  
                  
                  
                }
              }) #end observeEvent for button handling
            }#end if(n!=0)
            #end obsever generatePlot
            #end if for checking generate plot existence
          }) #end local for this_n
          
        } #end for this_n loop
        
      }) #end observe DV							
    }) #end local for this_item
    
  }	 #end for this_item loop
  
  cat(file=stderr(), "LOG: End generating plots\n")
  # RTF
  observeEvent(input$outputGo,{  
      observe(if(input$RTF & input$saveAs!="" ){
        
        cat(file=stderr(), "LOG: writing RTF\n")
        if(debug){
          message <- "writing RTF"
          input_nms <- names(input)
          input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
          names(input_vals) <- input_nms
          save(message,input_vals,guiGrobs,file=file.path(debugDir,"message.rda"))
        }
        ###############			
        #			make a document
        ###############	
        Dir=sprintf("%s/%s_%s/", currentWD(), gsub("[[:space:]]|\\.", "_", input$projectTitle), Sys.Date())
        dir.create(Dir,showWarning=FALSE)
        fileHead=sprintf("%s%s_%s",Dir, input$saveAs, Sys.Date())
        grobFile=sprintf("%s_Grobs.R", fileHead)
        
        getObj <- function(x){
          titles <- lapply(grep("LegendTitle",names(input)), function(xx) input[[xx]])
          names(titles) <- grep("LegendTitle",names(input),value=T)
          str_split(names(which(x == titles)),"LegendTitle")[[1]][2]
        }
        
        figureOrder <- tryCatch(sapply(input$figureOrder, getObj) ,
                                error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","figureOrderError.rda")) )
        tableOrder <- tryCatch(sapply(input$tableOrder, getObj) ,
                             error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","tableOrderError.rda")))
        listingOrder <- tryCatch(sapply(input$listingOrder, getObj), 
                               error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","ListingOrderError.rda")) )
        guiGrobs <- tryCatch(guiGrobs[c(tableOrder,figureOrder,listingOrder)],
                             error=function(e) if(debug) save(e,file=file.path(srcDir,"tkmp","guiGrobsOrderError.rda")))
        
        tryCatch(writeRTF(grobFile),
                 error=function(e){
                   cat(file=stderr(), paste("LOG: failed to write RTF\n",e,"\n"))
                   if(debug){
                     save(guiGrobs, grobFile, figureOrder, tableOrder, listingOrder, file=file.path(srcDir,"tmp","rtferror.rda"))
                   }
                 })
      })  
  })
  
  observeEvent(input$newTemplateGo,{  
    cat(file=stderr(), "LOG: newTemplateGo\n")
     if(input$saveTemplateAs!=""){
         for(item in names(DefaultsFirst)){
           #put in the non-plot related Defaults
           if (item %nin% c(
             "saveAll",
             "saveAs",
             "saveParm",
             "saveAsParm",
             "PNG",
             "RTF",
             "dataPath", 
             "recall"))
           {Defaults[[item]]<<-input[[item]]}
         }
       if(debug){
         input_nms <- names(input)
         input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
         names(input_vals) <- input_nms
         message <- "DEBUG Y"
         save(message, Defaults, input_vals, file=file.path(debugDir,"message.rda"))
       }
       tryCatch(recordInput(input=input,Defaults=Defaults,currentWD=currentWD()),
                warning=function(w) cat(file=stderr(), paste("LOG: recordInput warning\n",w)),
                error=function(e)  cat(file=stderr(), paste("LOG: recordInput error\n",e))
       )
     }
  })
  
  
  #Output and Saving Tabset  
  
  output$SaveTabset<-renderUI({
    cat(file=stderr(), paste("LOG: saveTabset\n"))
    wellPanel(
      checkboxInput("PNG", "Record *.pngs", Defaults$PNG),
      checkboxInput("RTF", "Construct *.Doc", Defaults$RTF),
      checkboxInput("verbose", "Reveal Function Text?", Defaults$verbose),
      textInput("saveAs", "File Name", Defaults$saveAs),
      actionButton("outputGo", "Save"),
      h1(),
      textInput("saveTemplateAs", "Template Name", Defaults$saveTemplateAs),
      actionButton("newTemplateGo", "Save")
      
    )
    
  
  })
  #End Shiny Server
})
