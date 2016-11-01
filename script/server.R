debug <- T
# Intro ----
#rm(list=ls(all=TRUE))
Sys.setenv(PATH=paste0(Sys.getenv("PATH"),":/usr/bin:/usr/lib/rstudio-server/bin")) # Get pandoc and imagemagick
srcDir <- "/data/tflgenerator"
if(!dir.exists(srcDir)) srcDir <- "/data/co/tflgenerator"
root <- ifelse(
  dir.exists("/opt/NMStorage_uslv"),
  "/opt/NMStorage_uslv",
  file.path(srcDir,"NMStorage")) # shinyFiles requires starting point for browser, we know this exists.
if(debug){
  debugDir <- file.path(srcDir,"tmp")
  dir.create(debugDir)
}

cat(file=stderr(), paste0("LOG: ", Sys.time(), " Start loading packages"))

.libPaths(file.path(srcDir,"script/lib"))


library(grid)
library(GUI)
library(TFL) # the Amgen Internal TFL package
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyBS)
library(DT)
library(animation)
library(lazyeval)
library(gtools)
library(readr)
library(shinyAce)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(colourpicker)
library(plyr)
library(dplyr)
#library(shinyjs)
#library(shinyBS)

cat(file=stderr(), paste0("LOG: ", Sys.time(), " Finished preamble\n"))
pListGlobal=list()

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " Entering sourcing of shinyServer function\n"))
  if(exists("originalTableData",envir=.GlobalEnv)){
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " !!! Using pre-existing originalTableData !!!\n") )
    rm("originalTableData",envir = .GlobalEnv)
  }
  if(exists("originalSourceData",envir=.GlobalEnv)){
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " !!! Using pre-existing originalSourceData !!!\n") )
    rm("originalSourceData",envir = .GlobalEnv)
  }
  
  
  if(!exists("Defaults",envir=.GlobalEnv)){
    tryCatch(Defaults<<-DefaultsFirst,
             error=function(e){
               cat(file=stderr(),paste0("LOG: ", Sys.time(), " Waiting for DefaultsFirst to load\n"))
               cat(file=stderr(),paste("Search path:\n",search(),"\n\n"))
               data(DefaultsFirst)
               Defaults <<- DefaultsFirst
             })
  }else{
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " !!! Using pre-existing global environment Defaults !!!\n"))
    defs <- get("Defaults",envir=.GlobalEnv)
    if(debug) save(Defaults, file=file.path(srcDir,"tmp/globalDefaults.rda"))
  }
  # tryCatch(unlockBinding("tabList", as.environment("package:GUI")),
  #          error=function(e){
  #            cat(file=stderr(),paste0("LOG: ", Sys.time(), " Waiting on tabList\n")
  #            cat(file=stderr(),paste("Search path:\n",search()))
  #            system("sleep 5")
  #            unlockBinding("tabList", as.environment("package:GUI"))
  #          })
  # tabList<<-tabList()
  
  
  
  # Get client data
  # cdata <- session$clientData
  # 
  # # Values from cdata returned as text
  # output$clientDataText <- renderText({
  #   cat(file=stderr(), paste0("LOG: ", Sys.time(), " clientDataText\n")
  #   cnames <- names(cdata)
  #   
  #   allvalues <- lapply(cnames, function(name) {
  #     paste(name, cdata[[name]], sep=" = ")
  #   })
  #   paste(allvalues, collapse = "\n")
  # })
  
  # Open Template ----
  observeEvent(input$templateGo,{
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " templateGo\n"))
    inFile <- input$templatePath
    if (is.null(inFile))
      return(NULL)
    source(inFile$datapath)
  }) 
  
  observe(if(TRUE){  	
    inFile <- input$templatePath
    if (is.null(inFile))
      return(NULL)
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " loading template file\n"))
    source(inFile$datapath, local=T)
    # For backward compatibility, impute Default values that are new
    missingvars <- pool(names(Defaults),names(DefaultsFirst))$y
    for(nm in missingvars) Defaults[[nm]] <- DefaultsFirst[[nm]]
    Defaults <<- Defaults
    # Now we need to reset all input, force a refresh
    session$reload()
  })
  
  
  # Setting Color Schemes ----
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " Setting the color schemes\n"))
  unlockBinding("cleanScales", as.environment("package:TFL"))
  # Set color scale.  In the future, remove this and activate checkbox on frontpage to enable grayscale
  cleanScales<<-setColorScale(shapeList = shape_pal()(6))
  
  observe(
    if("Color" %in% names(input)){
      if(input$Color){
        cat(file=stderr(), paste0("LOG: ", Sys.time(), " setColorScale\n"))
        cleanScales<<-setColorScale()}
    }
  )
  
  observe(
    if("Color" %in% names(input)){
      if(!input$Color){
        cat(file=stderr(), paste0("LOG: ", Sys.time(), " setGrayScale\n"))
        cleanScales<<-setGrayScale()}
    }
  )
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " Choosing shinyDir\n"))
  shinyDirChoose(input, id="dataPath", session=session, roots=c(NMStorage=root))
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " shinyDir chosen\n"))
  
  # output$dataPath <- currentWD()
  output$dataPath <- renderText({currentWD()})
  
  # currentWD <- reactive(
  #   if("dataPath" %nin% names(input)){
  #     if(("manualDataPath" %in% names(input)) & ("manualDataPath" != Defaults["manualDataPath"])){
  #         workingDirectory <- input[["manualDataPath"]]
  #         cat(file=stderr(), paste0("LOG: ", Sys.time(), " setting working directory to manualDataPath\n"))
  #         return(workingDirectory)
  #       }else{
  #         cat(file=stderr(), paste0("LOG: ", Sys.time(), " Using default dataPath\n"))
  #         return(Defaults$dataPath)
  #       }
  #   }else{
  #     # The user elected to use the shinyfiles widget
  #     cat(file=stderr(), paste0("LOG: ", Sys.time(), " using shinyFiles widget for working directory\n"))
  #     workingDirectory <- parseDirPath(roots=c(NMStorage=root),input$dataPath)
  #     return(workingDirectory)
  #   }
  # )
  currentWD <- reactive(
    if("manualDataPath" %in% names(input)){
      if(input$manualDataPath!=""){
        workingDirectory <- input[["manualDataPath"]]
        cat(file=stderr(), paste0("LOG: ", Sys.time(), " setting working directory to manualDataPath\n"))
        return(workingDirectory)
      }else{
        if(dir.exists(Defaults$manualDataPath)){
          cat(file=stderr(), paste0("LOG: ", Sys.time(), " Using default dataPath\n"))
          return(Defaults$manualDataPath)
        }else{
          Defaults$manualDataPath <- Sys.getenv("HOME")
          cat(file=stderr(), paste0("LOG: ", Sys.time(), " Using home directory for wd\n"))
          return(Defaults$manualDataPath)
        }
      }
    }else{
      if(dir.exists(Defaults$manualDataPath)){
        cat(file=stderr(), paste0("LOG: ", Sys.time(), " Using default dataPath\n"))
        return(Defaults$manualDataPath)
      }else{
        Defaults$manualDataPath <- Sys.getenv("HOME")
        cat(file=stderr(), paste0("LOG: ", Sys.time(), " Using home directory for wd\n"))
        return(Defaults$manualDataPath)
      }
    }
  )
  
  
  readThis <- reactive({
    list(
      # user=Sys.getenv("USER"),
      # home=Sys.getenv("HOME"),
      # runno=input$runno,
      # srcData=input$srcData,
      # ext=input$ext,
      # currentWD=currentWD(),
      # manualDataPath=input$manualDataPath,
      # templatePath_name=input$templatePath$name,
      # templatePath_datapath=input$templatePath$datapath,
      # header=input$header,
      # skipLines=input$skipLines,
      # dataLimits=input[["dataLimits"]],
      # dataTrans=input[["dataTrans"]],
      sessionInfo=sessionInfo()
    )
  })
  
  output$readThis <- renderPrint({readThis()})
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " Entering initial/external interactions\n"))
  # Initial/external interactions ----
  
  
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
  
  #read data in a reactive format
  tableFile=reactive({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " tableFile called\n"))
    
    if("runno" %nin% names(input)) return()
    
    if(input$runno=="#"){
      return()
    }
    
    
    withProgress(message="Loading...", value=.25, {
      
      if(input$runno!="#"){
        
        extensions=unlist(str_split(input$ext, ","))
        extensions=gsub("[[:space:]]*", "", extensions)
        
        runs=input$runno
        runs=unlist(str_split(input$runno, ","))
        runs=gsub("[[:space:]]*", "", runs)
        
        if(!exists("originalTableData",envir=.GlobalEnv)){
          dat=matrix()
          for(irun in runs) {
            for(iext in extensions){
              ext=iext
              fileName=sprintf("%s/%s/%s%s", currentWD(), irun, irun, ext)
              if(!file.exists(fileName)){
                return()
              }			
              #foo=try(read.table(fileName, header=input$header, skip=input$skipLines, stringsAsFactors=F, fill=TRUE, comment.char="",check.names=F))
              
              foonm=try(names(read.table(fileName, skip=input$skipLines,nrows=1,header=T)))
              foo=try(read_delim(fileName, col_names=foonm, skip=input$skipLines+1,delim=" ",comment=""))
              if(class(foo)=="try-error") return()
              if(nrow(foo)==0) return()
              foo$Run=irun
              dat=merge(dat, foo, all=TRUE)
              dat=dat[rowSums(is.na(dat)) != ncol(dat),]
              dat$V1=NULL
            }	
          }
          originalTableData <<- dat
        }
        incProgress(amount=1/length(runs))
      }
    })
    
    dat=try(data.frame(get("originalTableData",envir=.GlobalEnv), stringsAsFactors=F))
    if(class(dat)=="try-error") return()
    
    if("tableSubset" %in% names(input)){
      if(length(input[["tableSubset"]] > 0)){
        if(any(input[["tableSubset"]] != "")) dat <- dat[, setdiff(names(dat),input[["tableSubset"]])]
      }
      if(length(Defaults[["tableSubset"]] > 0)){
        if(any(Defaults[["tableSubset"]] != "")) dat <- dat[,setdiff(names(dat),Defaults[["tableSubset"]])]
      }
    }
    
    if("tableNA" %in% names(input)){
      tableNA <- unlist(strsplit(input$tableNA,","))
      tableNAnum <- as.numeric(tableNA)
      tableNA <- tableNA[ !is.na(tableNA) ]
      tableNAnum <- tableNAnum[ !is.na(tableNAnum) ]
      for(j in 1:ncol(dat)){
        if(class(dat[,j])=="numeric"){
          dat[dat[,j]%in%tableNAnum,j] <- NA
        }else{
          dat[dat[,j]%in%tableNA,j] <- NA            
        }
      }
    }
    
    
    if("dataParse_table" %in% names(input)){
      parsecommands <- cleanparse(input[["dataParse_table"]],"dat")
      if(!is.null(parsecommands)){
        for(i in 1:length(parsecommands$commands)){
          if(parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- within(dat, {", parsecommands$commands[i], "})"))),
                                               error=function(e) cat(file=stderr(),paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e)))
          if(!parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- ", parsecommands$commands[i]))),
                                                error=function(e) cat(file=stderr(),print(paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e))))
        }
      }
    }
    
    # Update selectizeInputs
    updateSelectizeInput(session,"tableSubset",
                         choices= unique(c(Defaults[["tableSubset"]],names(dat))),
                         selected=Defaults[["tableSubset"]],
                         server=T)
    
    # For debugging, save a copy of input
    if(debug){
      if(!exists("dat")) return(NULL) else tabdat <- dat
      tabList <- get("tabList",envir=.GlobalEnv)
      save(tabdat,file=file.path(debugDir,"tabdat.rda"))
    }
    revals$nms_tab <- isolate(names(dat))
    updateSelectizeInput(session,"mergeKey",choices=intersect(revals$nms_tab,revals$nms_source),selected=Defaults[["mergeKey"]],server=T)
    # End debugging
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " tableFile finished successfully\n"))
    return(dat)
  })	
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " End dataFile definition\n"))
  
  # Read data in a reactive format ----
  sourceFile=reactive({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " sourceFile called\n"))
    
    if("srcData" %nin% names(input)) return()
    
    if(input$srcData%in%c("sourcedata.csv",""," ","  ","   ")){
      return()
    }
    
    withProgress(message="Loading...", value=.25, {
      
      srcDatFile=sprintf("%s/%s", currentWD(), input$srcData)
      if(!file.exists(srcDatFile)){
        return()
      }
      if(!exists("originalSourceData",envir=.GlobalEnv)){
        originalSourceData <<- try(as.best(read_csv(srcDatFile)))
        if(any(class(originalSourceData)=="try-error")){
          rm("originalSourceData",envir = .GlobalEnv)
          return()
        }
      }
      
      dat <- get("originalSourceData",envir = .GlobalEnv) # Point to a copy here
      
      dat=dat[rowSums(is.na(dat)) != ncol(dat),]
      incProgress(amount=.5)
      dat=data.frame(dat, stringsAsFactors=F)
      
      if("sourceSubset" %in% names(input)){
        if(length(input[["sourceSubset"]] > 0)){
          if(any(input[["sourceSubset"]] != "")) dat <- dat[,setdiff(names(dat),input[["sourceSubset"]])]
        }
        if(length(Defaults[["sourceSubset"]] > 0)){
          if(any(Defaults[["sourceSubset"]] != "")) dat <- dat[,setdiff(names(dat),Defaults[["sourceSubset"]])]
        }
      }
      
      if("sourceNA" %in% names(input)){
        sourceNA <- unlist(strsplit(input$sourceNA,","))
        sourceNAnum <- as.numeric(sourceNA)
        sourceNA <- sourceNA[ !is.na(sourceNA) ]
        sourceNAnum <- sourceNAnum[ !is.na(sourceNAnum) ]
        for(j in 1:ncol(dat)){
          if(class(dat[,j])=="numeric"){
            dat[dat[,j]%in%sourceNAnum,j] <- NA
          }else{
            dat[dat[,j]%in%sourceNA,j] <- NA            
          }
        }
      }
      
      if("dataParse_source" %in% names(input)){
        parsecommands <- cleanparse(input[["dataParse_source"]],"dat")
        if(!is.null(parsecommands)){
          for(i in 1:length(parsecommands$commands)){
            if(parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- within(dat, {", parsecommands$commands[i], "})"))),
                                                 error=function(e) cat(file=stderr(),paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e)))
            if(!parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- ", parsecommands$commands[i]))),
                                                  error=function(e) cat(file=stderr(),print(paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e))))
          }
        }
      }
    })
    
    # Update selectizeInputs
    updateSelectizeInput(session,"sourceSubset",
                         choices= unique(c(Defaults[["sourceSubset"]],names(dat))),
                         selected=Defaults[["sourceSubset"]],
                         server=T)
    
    # For debugging, save a copy of input
    if(debug){
      if(exists("dat")) sourcedat <- dat else return(NULL)
      tabList <- get("tabList",envir=.GlobalEnv)
      global <- ls(envir = .GlobalEnv)
      save(sourcedat,tabList,global,Defaults,file=file.path(debugDir,"sourcedat.rda"))
    }
    revals$nms_source <- isolate(names(dat))
    updateSelectizeInput(session,"mergeKey",
                         choices=intersect(revals$nms_tab,revals$nms_source),
                         selected=Defaults[["mergeKey"]],server=T)
    # End debugging
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " sourceFile finished successfully\n"))
    return(dat)
  })	
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " End sourceFile definition\n"))
  
  
  
  
  
  #############################################################################
  revals <- reactiveValues(nms_source="",nms_tab="",nms_subj="",nms_obs="",nms_df="")
  
  
  #read data in a reactive format
  dataFile=reactive({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " dataFile called\n"))
    
    if(is.null(tableFile()) & is.null(sourceFile())){
      return()
    }
    
    if(!is.null(tableFile()) & !is.null(sourceFile())){
      dat <- merge(sourceFile(), tableFile(), 
                   by=input[["mergeKey"]],
                   all.x=input[["keepAllSource"]],
                   all.y=input[["keepAllRun"]],
                   sort=F,suffixes=c(".source",".table")
      ) 
    }else {
      if(!is.null(tableFile())) dat <- tableFile() else dat <- sourceFile()
    }
    
    updateSelectizeInput(session,"DVCol",choices=names(dat),selected=Defaults[["DVCol"]],server=T)
    updateSelectizeInput(session,"TAFDCol",choices=names(dat),selected=Defaults[["TAFDCol"]],server=T)
    updateSelectizeInput(session,"STUDYCol",choices=names(dat),selected=Defaults[["STUDYCol"]],server=T)
    updateSelectizeInput(session,"NMIDCol",choices=names(dat),selected=Defaults[["NMIDCol"]],server=T)
    updateSelectizeInput(session,"IPREDCol",choices=names(dat),selected=Defaults[["IPREDCol"]],server=T)
    updateSelectizeInput(session,"PREDCol",choices=names(dat),selected=Defaults[["PREDCol"]],server=T)
    
    # dat=data.frame(dat, stringsAsFactors=TRUE)
    if("DVCol" %in% names(input)) names(dat)[which(names(dat)==input[["DVCol"]])] = "DV"
    if("TAFDCol" %in% names(input)) names(dat)[which(names(dat)==input[["TAFDCol"]])]="TAFD"
    if("NMIDCol" %in% names(input)) names(dat)[which(names(dat)==input[["NMIDCol"]])]="NMID"
    if("STUDYCol" %in% names(input)) names(dat)[which(names(dat)==input[["STUDYCol"]])]="STUDY"
    if("IPREDCol" %in% names(input)) names(dat)[which(names(dat)==input[["IPREDCol"]])]="IPRED"
    if("PREDCol" %in% names(input)) names(dat)[which(names(dat)==input[["PREDCol"]])]="PRED"
    
    if(input$sortBy){
      if(all(c("DV","TAFD","NMID")%in%names(dat))){
        if("STUDY" %in% names(dat)) dat <- dat[order(dat$STUDY,dat$NMID,dat$TAFD),] else dat <- dat[order(dat$NMID,dat$TAFD),]
      }
    }# else we assume the user has done so
    
    if("dataParse_analysis" %in% names(input)){
      parsecommands <- cleanparse(input[["dataParse_analysis"]],"dat")
      if(!is.null(parsecommands)){
        for(i in 1:length(parsecommands$commands)){
          if(parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- within(dat, {", parsecommands$commands[i], "})"))),
                                               error=function(e) cat(file=stderr(),paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e)))
          if(!parsecommands$within[i]) tryCatch(eval(parse(text=paste0("dat <- ", parsecommands$commands[i]))), 
                                                error=function(e) cat(file=stderr(),print(paste("Parsing command broken:\n",parsecommands$commands[i],"\n", e))))
        }
      }
    }
    
    dat0 <- dat
    
    updateSelectizeInput(session,"subjectExclusion_col",choices=names(dat),selected=Defaults[["subjectExclusion_col"]],server=T)
    updateSelectizeInput(session,"observationExclusion_col",choices=names(dat),selected=Defaults[["observationExclusion_col"]],server=T)
    
    
    if("subjectExclusion_col" %in% names(input)){
      if(input[["subjectExclusion_col"]]%in%names(dat)) revals$nms_subj <- isolate(unique(dat[,input$subjectExclusion_col]))
    }
    if("observationExclusion_col" %in% names(input)){
      if(input[["observationExclusion_col"]]%in%names(dat)) revals$nms_obs <- isolate(unique(dat[,input$observationExclusion_col]))
    }
    
    # Can we calculate whole subject exclusions yet?
    defs <- grep("subjectExclusion[[:digit:]]",names(Defaults),value=T)
    if(length(defs)>0){
      
      # Get the exclusions inputs, assign to Defaults for autosave
      for(excl in grep("subjectExclusion",names(input),value=T)){
        if(!grepl("contentsHead",excl))  Defaults[[excl]] <<- input[[excl]]
      }
      
      codes <- sapply(defs, function(x) str_trim(strsplit(Defaults[[x]],"::",fixed=T)[[1]][1]))
      reasons <- sapply(defs, function(x) str_trim(strsplit(Defaults[[x]],"::",fixed=T)[[1]][2]))
      codes <- codes[!is.na(reasons)]
      reasons <- reasons[!is.na(reasons)]
      if(length(reasons)>0){
        if(any(reasons!="")){
          cat(file=stderr(), paste0("LOG: ", Sys.time(), " calculating subject exclusions\n"))
          if(debug){
            save(defs,codes,reasons,Defaults,file=file.path(srcDir,"tmp/defs_subj.rda"))
          }
          
          excldat <- dat[dat[,input[["subjectExclusion_col"]]] %in% codes, ]
          
          # Ensure there are no patients in the excluded dataset left
          if("NMID" %in% names(dat)){
            if(c("STUDY" %in% names(dat))){
              ids <- unique(paste(excldat$NMID, excldat$STUDY))
              subjectExclusions <- dat[paste(dat$NMID,dat$STUDY)%in%ids,]
              dat <- dat[paste(dat$NMID,dat$STUDY)%nin%ids,]
            }else{
              ids <- unique(paste(excldat$NMID))
              subjectExclusions <- dat[paste(dat$NMID) %in% ids,]
              dat <- dat[paste(dat$NMID)%nin%ids,]
            }
            subjectExclusions$excl_reasons <- 
              metrumrg:::map(subjectExclusions[,input[["subjectExclusion_col"]]],
                             codes, reasons)
            #Make this available everywhere
            subjectExclusions <<- subjectExclusions
          }
        }
      }
    }
    
    # Can we calculate observation exclusions yet?
    defs <- grep("observationExclusion[[:digit:]]",names(Defaults),value=T)
    if(length(defs)>0){
      # Get the exclusions inputs, assign to Defaults for autosave
      for(excl in grep("observationExclusion",names(input),value=T)){
        if(!grepl("contentsHead",excl)) Defaults[[excl]] <<- input[[excl]]
      }
      
      if(debug){
        save(defs,Defaults,file=file.path(srcDir,"tmp/defs_obs.rda"))
      }
      codes <- sapply(defs, function(x) str_trim(strsplit(Defaults[[x]],"::",fixed=T)[[1]][1]))
      reasons <- sapply(defs, function(x) str_trim(strsplit(Defaults[[x]],"::",fixed=T)[[1]][2]))
      codes <- codes[!is.na(reasons)]
      reasons <- reasons[!is.na(reasons)]
      
      if(length(reasons)>0){
        if(any(reasons!="")){
          
          cat(file=stderr(), paste0("LOG: ", Sys.time(), " calculating observation exclusions\n"))
          
          defs <- grep("observationExclusion[[:digit:]]",names(Defaults),value=T)
          codes <- sapply(defs, function(x) strsplit(Defaults[[x]],"::",fixed=T)[[1]][1])
          reasons <- sapply(defs, function(x) strsplit(Defaults[[x]],"::",fixed=T)[[1]][2])
          codes <- codes[!is.na(reasons)]
          reasons <- reasons[!is.na(reasons)]
          
          observationExclusions <- dat[dat[,input[["observationExclusion_col"]]] %in% codes, ]
          dat <- dat[dat[,input[["observationExclusion_col"]]] %nin% codes,]
          
          observationExclusions$excl_reasons <- 
            metrumrg:::map(observationExclusions[,input[["observationExclusion_col"]]],
                           codes, reasons)
          #Make this available in global
          observationExclusions <<- observationExclusions
          
        }
      }
      
    }
    
    revals$nms_df <- isolate(names(dat))
    
    
    
    # For debugging, save a copy of input
    if(debug){
      dati <- dat
      tabList <- get("tabList",envir=.GlobalEnv)
      if(exists("subjectExclusions") & exists("observationExclusions")){
        save(dati,dat0,subjectExclusions,observationExclusions,Defaults,file=file.path(debugDir,"shinytmpdat.rda"))
      }else{
        save(dati,Defaults,file=file.path(debugDir,"shinytmpdat.rda"))
      }
    }
    # End debugging
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " dataFile finished successfully\n"))
    return(dat)
  })	
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " End dataFile definition\n"))
  
  
  # Output Renders - Just the data set overview and plot title----
  
  #Raw Contents
  observeEvent(input[["updateRunView"]],{
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " contentsHead_tabledata called\n"))
    autosave()
    output$contentsHead_tabledata <- DT::renderDataTable({
      req(input$runno, isolate(tableFile()))
      return(DT::datatable(isolate(tableFile()),filter="top"))
    })
  })
  observeEvent(input[["updateSourceView"]],{
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " contentsHead_sourcedata called\n"))
    autosave()
    output$contentsHead_sourcedata <- DT::renderDataTable({
      req(input$srcData,isolate(sourceFile()))
      return(DT::datatable(isolate(sourceFile()), filter="top"))
    })  
  })
  observeEvent(input[["performMerge"]],{
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " contentsHead_analysisdata called\n"))
    autosave()
    output$contentsHead_analysisdata <- DT::renderDataTable({
      return(DT::datatable(isolate(dataFile()), filter="top"))
    })  
  })
  
  output$contentsHead_subjectExclusions <- DT::renderDataTable({ NULL })
  
  
  
  output$contentsHead_observationExclusions <- DT::renderDataTable({NULL})
  observeEvent(input$generateobservationExclusions,{
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " contentsHead_observationExclusions called\n"))
    foo <- isolate(dataFile())
    output$contentsHead_observationExclusions <-
      DT::renderDataTable({DT::datatable(observationExclusions, filter="top")})
  })
  
  #Raw Contents
  summarizeContents <- function(data){
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " performing contents summary\n"))
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
  
  observeEvent(input$generatesubjectExclusions,{
    cat(file=stderr(),paste0("LOG: ", Sys.time(), " contentsHead_subjectExclusions called\n"))
    foo <- isolate(dataFile())
    output$contentsHead_subjectExclusions <- 
      DT::renderDataTable({DT::datatable(subjectExclusions, filter="top")})
  })
  # output$contentsSummary_subjectExclusions <- renderPrint(NULL)
  # observeEvent(exists("subjectExclusions"),{
  #   output$contentsSummary_subjectExclusions <- renderPrint({summarizeContents(subjectExclusions)})
  # })
  # output@contentsSummary_observationExclusions <- renderPrint(NULL)
  # observeEvent(exists("observationExclusions"),{
  #   output$contentsSummary_observationExclusions <- renderPrint({summarizeContents(observationExclusions)})
  # })
  
  
  output$projectTitle <- renderText({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " printing project title\n"))
    #This prints out the input project title to the appropriate GUI location
    input$projectTitle
  })
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " End data summary tab definitions\n"))
  #Data Input Tabset
  
  
  excl_list <- reactive({
    # if(req(input$generateExclusions) == 0) return(list(h1(""),renderText("Press button to generate exclusions mapping")))
    # if("subjectExclusion_col" %nin% names(input)) return(list(renderPrint("Input exclusion column")))
    # if(input$subjectExclusion_col == "") return(list(renderPrint("Input exclusion column")))
    cat(file=stderr(),paste0("LOG: ", Sys.time(), " excl_list called\n"))
    #nms <- isolate(unique(dataFile()[,input$subjectExclusion_col]))
    nms <- paste0(revals$nms_subj,"::")
    # If there's no existing default, or if the exclusion indicator is different
    # for the exclusion, set it it to ""
    for(i in seq_along(nms)){
      Defaults[[paste0("subjectExclusion",i)]] <<- 
        ifelse(any(grepl(nms[i],Defaults,fixed=T)), 
               grep(nms[i],Defaults,fixed=T,value=T)[1],
               nms[i])
    }
    out <- lapply(seq_along(nms),
                  function(i){
                    id <- strsplit(nms[i],"::",fixed=T)[[1]][1]
                    textInput(paste0("subjectExclusion",i),
                              paste0("Exclusion reason for ",id, " (add reason after <id>::)"),
                              Defaults[[paste0("subjectExclusion",i)]])
                  })
    out <- c(out, list(h1("")),list(actionButton("generatesubjectExclusions","Generate subject exclusions")))
    return(out)
  })
  
  obsexcl_list <- reactive({
    # if(req(input$generateExclusions) == 0) return(list(h1(""),renderText("Press button to generate exclusions mapping")))
    # if("subjectExclusion_col" %nin% names(input)) return(list(renderPrint("Input exclusion column")))
    # if(input$subjectExclusion_col == "") return(list(renderPrint("Input exclusion column")))
    cat(file=stderr(),paste0("LOG: ", Sys.time(), " obsexcl_list called\n"))
    #nms <- isolate(unique(dataFile()[,input$observationExclusion_col]))
    nms <- paste0(revals$nms_obs,"::")
    # If there's no existing default, or if the exclusion indicator is different
    # for the exclusion, set it it to ""
    for(i in seq_along(nms)){
      Defaults[[paste0("observationExclusion",i)]] <<- 
        ifelse(any(grepl(nms[i],Defaults,fixed=T)), 
               grep(nms[i],Defaults,fixed=T,value=T)[1],
               nms[i])
    }
    out <- lapply(seq_along(nms),
                  function(i){
                    id <- strsplit(nms[i],"::",fixed=T)[[1]][1]
                    textInput(paste0("observationExclusion",i),
                              paste0("Exclusion reason for ",id, " (add reason after <id>::)"),
                              Defaults[[paste0("observationExclusion",i)]])
                  })
    out <- c(out,list(h1("")),list(actionButton("generateobservationExclusions","Generate observation exclusions")))
    return(out)
  })
  
  observeEvent(input$clearDataCache, { 
    cat(file=stderr(),"Clearing data cache")
    try(rm("originalTableData",envir=.GlobalEnv))
    try(rm("originalSourceData",envir=.GlobalEnv))
    try(rm("subjectExclusions",envir=.GlobalEnv))
    try(rm("observationExclusions",envir=.GlobalEnv))
    try(revals$nms_subj <- revals$nms_obs <- "")
    Defaults <<- DefaultsFirst
  })
  
  output$projectInfoTabset <- renderUI({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating data input tabset\n"))
    tabsetPanel(
      tabPanel(title="Project Information",
               wellPanel(
                 textInput(inputId="projectTitle", label="Project Title:", value=Defaults$projectTitle),
                 boxInputLarge(inputId="projectInfo", label="Project Information:", value=Defaults$projectInfo)
               )
      ),
      tabPanel(title="Model Info",
               wellPanel(
                 textInput(inputId="manualDataPath", label="Parent working directory:", value=Defaults$manualDataPath),	
                 textInput(inputId="srcData", label='NONMEM source data:',value=Defaults$srcData),
                 textInput(inputId="runno", label="Run Number:", value=Defaults$runno),
                 #textInput(inputId="numModel", label="Number of Models", value="1"),
                 textInput(inputId="ext", label="File Extensions:", value=Defaults$ext),
                 checkboxInput('header', 'Header?', value=Defaults$header),
                 numericInput("skipLines", "Skip Lines:", value=Defaults$skipLines),
                 actionButton("clearDataCache","Clear cached data")
                 #,
                 #textInput(inputId="baseModel", label="Base Model #", value="")
               )
      )
      # ,
      # tabPanel(title="Change E-R SSAP Defaults",
      #          wellPanel( 
      #            textInput("DVCol", "DV Column", Defaults$DVCol),
      #            textInput("TAFDCol", "TAFD Column", Defaults$TAFDCol),
      #            textInput("STUDYCol", "STUDY Column", Defaults$STUDYCol),
      #            textInput("NMIDCol", "NMID Column", Defaults$NMIDCol),
      #            textInput("IPREDCol", "IPRED Column", Defaults$IPREDCol),
      #            textInput("PREDCol", "PRED Columns", Defaults$PREDCol),
      #            textInput("subjectExclusion_col", "Subject exclusion column\n(analysis dataset)",Defaults$subjectExclusion_col),
      #            textInput("observationExclusion_col", "Observation exclusion column\n(analysis dataset)",Defaults$observationExclusion_col)
      #          )
      # )
      
    )
  })
  
  
  #Data Tabset  
  output$DataTabset <- renderUI({		
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating data view tabset \n"))
    #The first PanelSet is what is loaded with the base defaults.  
    PanelSet=list(
      tabPanel("Source Data",
               fluidRow(
                 column(width = 6, title="Code parser", 
                        h4("Manipulation code"),
                        aceEditor("dataParse_source", value=Defaults[["dataParse_source"]], mode="r", theme="chrome", wordWrap=T)
                 ),
                 column(width = 6, title="Subsetting",
                        h2(""),
                        actionButton("updateSourceView", "View/refresh data"),
                        # selectizeInput("sourceSubset", "Choose source columns to drop",
                        #                choices= isolate(names(sourceFile())),
                        #                selected=Defaults[["sourceSubset"]],
                        #                multiple=T),
                        selectizeInput("sourceSubset", "Columns to drop", choices=Defaults[["sourceSubset"]], selected=Defaults[["sourceSubset"]], multiple=T),
                        h2(""),
                        textInput("sourceNA",label="Comma separated list of missingness identifiers",
                                  value=Defaults$sourceNA)
                 )
               ),
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
      tabPanel("Run Data",
               fluidRow(
                 column(width = 6, title="Code parser (table)", 
                        h3("Manipulation code"),
                        aceEditor("dataParse_table", value=Defaults[["dataParse_table"]], mode="r", theme="chrome", wordWrap=T)
                 ),
                 column(width = 6, title="Subsetting (table)",
                        h2(""),
                        actionButton("updateRunView", "View / refresh data"),
                        # selectizeInput("tableSubset", "Choose run columns to drop",
                        #                choices= isolate(names(tableFile())),
                        #                selected=Defaults[["tableSubset"]],
                        #                multiple=T),
                        selectizeInput("tableSubset", "Columns to drop", choices=Defaults[["tableSubset"]], selected=Defaults[["tableSubset"]], multiple=T),
                        h2(""),
                        textInput("tableNA",label="Comma separated list of quoted missingness identifiers",
                                  value=Defaults$tableNA)
                 )
               ),
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
      tabPanel("Manipulation code examples",
               fluidPage(
                 h1(""),
                 h4("Tips:"),
                 p("You can use the code parser to subset on values.  Use the variable $DATA to refer to the internal source dataset when doing so."),
                 p(""),
                 p("For example:"),
                 code("subset($DATA, STUDY==1 & CMT==4)"),
                 p(""),
                 p("You can also create factor labels with the parser:"),
                 code('SEXF <- factor(SEX, levels=c(0,1), labels=c("F","M"))'),
                 p(""),
                 p("You can also use it to calculate within keys of the data.  Use dplyr conventions for this:"),
                 code('group_by($DATA, NMID, cycle) %>% mutate(Cmin=min(DV[EVID==0]),Cmax=max(DV[EVID==0]))'),
                 p(""),
                 p("Use the dplyr format for fast sorting as well (use desc() to denote descending sorts):"),
                 code("group_by($DATA, STUDY, NMID) %>% arrange(STUDY, desc(NMID), TIME)")
               )
      )
      
    )
    dummy=(do.call(tabsetPanel, PanelSet))
    return(dummy)
    
  })
  
  #Data Tabset  
  output$aDataTabset <- renderUI({		
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating analysis data view tabset \n"))
    #The first PanelSet is what is loaded with the base defaults.  
    PanelSet=list(
      tabPanel("Analysis Data",
               fluidRow(
                 column(width = 6, title="Code parser (analysis)", 
                        h4("Manipulation code"),
                        aceEditor("dataParse_analysis", value=Defaults[["dataParse_analysis"]], mode="r", theme="chrome", wordWrap=T)
                        # bsPopover("dataParse_analysis", "manipHelpPop", 
                        #           content = paste0(
                        #             c(
                        #               "Tips:\n",
                        #               "Subset on values:\n",
                        #               "subset($DATA, STUDY==1 & CMT==4)\n",
                        #               "Factor labels:\n",
                        #               'SEXF <- factor(SEX, levels=c(0,1), labels=c("F","M"))'
                        #             )
                        #           ),
                        #           
                        #           trigger = 'hover',
                        #           placement="bottom")
                 ),
                 column(width=6, title="Analysis data specification",
                        fluidRow(column(width=12, title="Action button",
                                        actionButton("performMerge", "Perform merge, update data view")
                        )),
                        wellPanel(
                          fluidRow(
                            column(width = 6, title="Merge specification",
                                   selectizeInput("mergeKey", "Choose merge key", 
                                                  choices=intersect(revals$nms_source,revals$nms_tab),
                                                  multiple=T,
                                                  selected=Defaults[["mergeKey"]])
                                   # selectizeInput("mergeKey", "Choose merge key",
                                   #                choices= intersect(names(sourceFile()),names(tableFile())),
                                   #                selected=Defaults[["mergeKey"]],
                                   #                multiple=T)
                                   # selectizeInput("sortBy", "Sort analysis data by:",
                                   #                choices=c(revals$nms_source,revals$nms_tab),
                                   #                selected=Defaults[["sortBy"]],
                                   #                multiple=T
                                   #                )
                            ),
                            column(width = 6, title="Merge specification (2)",
                                   checkboxInput("keepAllSource", "Retain all source values?",Defaults[["keepAllSource"]]),
                                   checkboxInput("keepAllRun", "Retain all table file values?", Defaults[["keepAllRun"]]),
                                   checkboxInput("sortBy", "Sort by study, patient, and time?",value=T)
                            )
                          ),
                          fluidRow(
                            column(12,
                                   p("If not sorting, you MUST specify sorting order in the data manipulator")
                            )
                          )
                        ),
                        fluidRow(
                          column(width=6,title="Change defaults",
                                 wellPanel(
                                   selectizeInput("DVCol","DV Column",choices=Defaults[["DVCol"]],selected=Defaults[["DVCol"]]),
                                   selectizeInput("TAFDCol","TAFD Column",choices=Defaults[["TAFDCol"]],selected=Defaults[["TAFDCol"]]),
                                   selectizeInput("STUDYCol","STUDY Column",choices=Defaults[["STUDYCol"]],selected=Defaults[["STUDYCol"]]),
                                   selectizeInput("NMIDCol","NMID Column",choices=Defaults[["NMIDCol"]],selected=Defaults[["NMIDCol"]])
                                 )
                          ),
                          column(width=6,title="Change defaults",
                                 wellPanel(
                                   selectizeInput("IPREDCol","IPRED Column",choices=Defaults[["IPREDCol"]],selected=Defaults[["IPREDCol"]]),
                                   selectizeInput("PREDCol","PRED Column",choices=Defaults[["PREDCol"]],selected=Defaults[["PREDCol"]]),
                                   selectizeInput("subjectExclusion_col","Subject exclusion column",choices=Defaults[["subjectExclusion_col"]],selected=Defaults[["subjectExclusion_col"]]),
                                   selectizeInput("observationExclusion_col","Observation exclusion column",choices=Defaults[["observationExclusion_col"]],selected=Defaults[["observationExclusion_col"]])
                                 )
                          )
                        )
                 )
               )       ,
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
      
    )
    dummy=(do.call(tabsetPanel, PanelSet))
    return(dummy)
    
  })  
  
  # observeEvent(input$evalsourceParse,{
  #   output$sourceParseCode <- renderPrint({
  #     input$evalsourceParse
  #     return(isolate(input[["dataParse_source"]]))
  #   })
  # })
  
  
  
  output$DataExcTabset <- renderUI({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating data exclusions tabset\n"))
    tabsetPanel(
      tabPanel(
        title="Subject exclusions",
        tabsetPanel(
          tabPanel(title="Subject exclusion specification",
                   h1(""),
                   do.call(wellPanel,excl_list())
          ),
          tabPanel("Subject Exclusion Data",
                   fluidRow(
                     column(width = 12,
                            box(
                              title = "", width = NULL, status = "primary",
                              div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsHead_subjectExclusions'))
                            )
                     )
                   )
          )
          # ,
          # tabPanel("Subject Exclusions Data Summary", verbatimTextOutput("contentsSummary_subjectExclusions"))
        )
      ),
      tabPanel(
        title="Observation exclusions",
        tabsetPanel(
          
          tabPanel(title="Observation exclusion specification",
                   h1(""),
                   do.call(wellPanel,obsexcl_list())
          ),
          tabPanel("Observation Exclusion Data",
                   fluidRow(
                     column(width = 12,
                            box(
                              title = "", width = NULL, status = "primary",
                              div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsHead_observationExclusions'))
                            )
                     )
                   )
          )
          # ,
          # tabPanel("Observation Exclusions Data Summary", verbatimTextOutput("contentsSummary_observationExclusions"))
        )
      )
    )
  })
  
  # Analysis Selection ----
  output$PlotTabset<-renderUI({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating analysis selection tabset\n"))
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
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating figures tabset \n"))
    type="Figures"
    types=grep(type, plotList$sidebarType)
    PanelSet=list()
    for (item in plotList$type[types]){
      if(paste(item, "Num", sep="") %in% names(input)){
        if("varNames" %in% names(formals(plotList$Call[plotList$type==item]))){
          PanelSet=do.call(what=plotList$Call[plotList$type==item], args=list(plotType=item, input=input, Set=PanelSet, varNames=revals$nms_df))
        }else{
          PanelSet=do.call(what=plotList$Call[plotList$type==item], args=list(plotType=item, input=input, Set=PanelSet ))
        }
      }
    }
    
    tabList <- do.call(tabsetPanel, PanelSet)
    
  })
  
  output$listingsTabset<-renderUI({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating listings tabset\n"))
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
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating tables tabset\n"))
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
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " creating current TFL tabset\n"))
    outList <- data.frame()
    for(item in plotList$type){
      numbers=as.numeric(unlist(str_extract_all(input[[paste(item, "Num", sep="")]], "\\d+")))
      if(length(numbers)>1){numRange=numbers[which(numbers!=0)]}
      if(length(numbers)==1){numRange=c(0:numbers)}
      if(length(numbers)==0){numRange=0}
      numRange <- setdiff(numRange,0)
      for(n in numRange){
        outList <- rbind(outList,data.frame(object=paste0(item,n), 
                                            title=input[[paste0("LegendTitle",item,n)]],
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
          selected=Defaults[["figureOrder"]],
          multiple=T, options=list(create=F)
        ),
        selectizeInput(
          "tableOrder", label="Tables order", 
          choices=as.character(outList$Tables$title), 
          selected=Defaults[["tableOrder"]],
          multiple=T, options=list(create=F)
        ),
        selectizeInput(
          "listingOrder", label="Listings order", 
          choices=as.character(outList$Listings$title), 
          selected=Defaults[["listingOrder"]],
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
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " matching table order to that specified\n"))
    tryCatch(
      data.frame(Label=paste0("Table ", 1:length(input[["tableOrder"]])),
                 Title=input[["tableOrder"]]),
      error=function(e) print("Unspecified")
    )
  })
  
  output$tflOrder_figures <- renderPrint({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " matching figure order to that specified\n"))
    tryCatch(
      data.frame(Label=paste0("Figure ", 1:length(input[["figureOrder"]])),
                 Title=input[["figureOrder"]]),
      error=function(e) print("Unspecified")
    )
  })
  output$tflOrder_listings <- renderPrint({
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " matching listing order to that specified\n"))
    tryCatch(
      data.frame(Label=paste0("Listing ", 1:length(input[["listingOrder"]])),
                 Title=input[["listingOrder"]]),
      error=function(e) print("Unspecified")
    )
  })
  
  # Autosave routine -------------------------
  
  autosave <- function(){
    local({
      
      # Autosave routine
      if(dir.exists(currentWD()) & input[["projectTitle"]]!=""){
        
        cat(file=stderr(),paste0("LOG: ", Sys.time(), " Running autosave routine"))
        Defaults.autosave <- get("Defaults",envir = .GlobalEnv)
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
            "recall"
          ))
          {
            if(!is.null(input[[item]])){
              Defaults.autosave[[item]]<-input[[item]]
              Defaults[[item]]<<-input[[item]]
            }
          }
        }
        if(debug){
          input_vals <- reactiveValuesToList(input)
          save(Defaults.autosave, Defaults, input_vals, file=file.path(srcDir,"tmp","autosave-debug.rda"))
        }
        tryCatch(recordInput(input=reactiveValuesToList(input),Defaults=Defaults.autosave,currentWD=currentWD(),autosave=T),
                 warning=function(w) cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " autosave warning\n",w))),
                 error=function(e)  cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " autosave error\n",e)))
        )  
        cat(file=stderr(), paste0("LOG: ", Sys.time(), " Exiting autosave routine"))
        
      }
    })
  }
  
  
  # Generating Plots, internal and saving ---------------
  
  cat(file=stderr(), paste0("LOG: ", Sys.time(), " Begin generating plots\n"))
  for (this_item in plotList$type){
    
    local({
      item=this_item
      #Observe if any plots of that type have been assigned inputs
      observe(if(length(grep(paste("reset", item, sep=""), names(input)))>0){
        
        numbers=as.numeric(unlist(str_extract_all(input[[paste(item, "Num", sep="")]], "\\d+")))
        if(length(numbers)>1){numRange=numbers[which(numbers!=0)]}
        if(length(numbers)==1){numRange=c(0:numbers)}
        if(length(numbers)==0){numRange=0}
        
        testClick=which(sapply(names(input)[grepl(paste0('button',item),names(input))],function(x) input[[x]]>0))
        if(length(testClick)>0) numRange=numRange[testClick]
        
        for (this_n in numRange){
          local({
            n=this_n
            if(n!=0){	
              observeEvent(input[[paste("button",item,n,sep="")]],{ 
                
                autosave()                
                
                if(debug){
                  message <- "DEBUG A"
                  input_nms <- names(input)
                  input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                  names(input_vals) <- input_nms
                  save(message, input_vals,n,file=file.path(debugDir,"message.rda"))
                } 
                #check if the defaults/inputs for a plot have been created
                if(length(grep(paste(item, n,sep=""), names(input)))>0){
                  
                  cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " checking priors for", item,n, "\n")))
                  
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
                  
                  #if(sameAsDefault!=1){
                  if(T){
                    cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " creating", item, n, "\n")))
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
                      input_vals <- reactiveValuesToList(input)
                      names(input_vals) <- input_nms
                      
                      save(message,argList, input_vals, file=file.path(debugDir,"message.rda"))
                    } 
                    
                    
                    if(class(argList)!="try-error"){
                      callType=argList$callType
                      argList$callType=NULL
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
                      pListGlobal[[paste0('Plot',item,n)]]<<- tryCatch({
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
                      
                      
                    }else pListGlobal[[paste0('Plot',item,n)]]<<- arrangeGrob(textGrob(sprintf("You broke something \n%s",as.character(attr(argList,"condition")))))
                    
                    
                    if(debug){
                      message <- "DEBUG D"
                      save(message,file=file.path(debugDir,"message.rda"))
                    } 
                    
                    output[[paste("generated",item,n,sep="")]] <<- renderPrint({ Sys.time() })
                    
                    if(item %in% c("inputTable", "inputListing", "inputFigure", "inputListing_text",
                                   "observationExclusionsTab","subjectExclusionsTab",
                                   "observationExclusionsSummaryTab",
                                   "subjectExclusionsSummaryTab")){
                      if("src" %in% names(pListGlobal[[paste0('Plot',item,n)]])){
                        output[[paste("Plot",item,n,sep="")]] <<-
                          renderImage(pListGlobal[[paste0('Plot',item,n)]]$src,deleteFile=F)
                      }else{
                        output[[paste("Plot",item,n,sep="")]] <<- 
                          renderPrint({ print(head(pListGlobal[[paste0('Plot',item,n)]]$preview,n=input[[paste0("previewhead",item,n)]]),row.names=F)})
                      }
                    }else if(item %nin% c("demogTabCont","demogTabCat","NMTab")) {
                      
                      # Theme Editor# -----
                      output[[paste("Plot", item,n, sep="")]]<<-renderPlot({
                        runjs("console.log('trip1')")
                        do.call(pListPrint,pListGlobal[[paste0('Plot',item,n)]])
                      })
                      
                      pListGlobal[[paste0('TempPlot',item,n)]]<<-pListGlobal[[paste0('Plot',item,n)]]$pList[[1]]
                      theme.now=theme_get()
                      if(length(pListGlobal[[paste0('TempPlot',item,n)]]$theme)>0) {
                        theme.now=theme.now+pListGlobal[[paste0('TempPlot',item,n)]]$theme
                      }
                      pListGlobal[[paste0('Theme',item,n)]]<<-themeFetch(theme.now)
                      
                      if(debug) {
                        input.now=reactiveValuesToList(isolate(input))
                        isolate(
                          save(input.now,pListGlobal,file=file.path(debugDir,"prethemeEditor.rda"))
                        )
                      }           

                      # #Update Session Theme
                      # observeEvent(input[["SetThemeGlobal",title,n]],{
                      #   if(length(pListGlobal[[paste0('TempPlot',item,n)]]$theme)>0) theme.now=theme.now+pListGlobal[[paste0('TempPlot',item,n)]]$theme
                      #   theme_set(theme_get()%+replace%theme.now)
                      # })
                      # 
                      # #Update Grid Theme
                      # update.ThemeGrid=eventReactive(input$SetThemeGrid,{
                      #   p.now<<-pList.new[[1]]
                      #   if(length(p.now$theme)>0) theme.now=theme.now+p.now$theme
                      #   
                      #   for(i in 1:length(pList.new)) pList.new[[i]]<<- pList.new[[i]]+theme.now
                      #   
                      #   return(pList.new)
                      # })
                      # 
                      # #Populate Modal Elements
                      output[[paste0("popTheme",item,n)]]<<-renderUI({
                        bsModal(id = paste0("updateThemePopup",item,n), title = "Update Plot Theme", trigger = paste0("updateTheme",item,n), size = "large",

                                do.call(tabsetPanel,
                                        unlist(lapply(1:length(pListGlobal[[paste0('Theme',item,n)]]),FUN = function(j){
                                          if(themeListDepth(pListGlobal[[paste0('Theme',item,n)]][j])>2){
                                            list(themeMakePanel(pListGlobal[[paste0('Theme',item,n)]][j],item=item,n=n))
                                          }else{
                                            unlist(lapply(j, function(i) {themeMakePanel(pListGlobal[[paste0('Theme',item,n)]][i],item=item,n=n)}),F)}
                                        }),F)


                                ),
                                hr(),
                                actionButton(inputId = paste0("setTheme",item,n),label = "Set Theme")
                        )
                      })
                      
                      #Update Theme
                      observeEvent(input[[paste0('setTheme',item,n)]],{
                        if(debug) {
                          input.now=reactiveValuesToList(isolate(input))
                          isolate(
                            save(input.now,pListGlobal,file=file.path(debugDir,"setTheme.rda"))
                          )
                        }
                        
                        strThemeCallList=lapply(names(pListGlobal[[paste0('Theme',item,n)]]),function(item0){
                          themeNewVal(item,n,pListGlobal[[paste0('Theme',item,n)]][item0],pListGlobal[[paste0('TempPlot',item,n)]],input)
                        })

                        strThemeCall=paste0("pListGlobal[['TempPlot",item,n,"']]<<-pListGlobal[['TempPlot",item,n,"']]+theme(",paste0(unlist(strThemeCallList),collapse = ","),")")
                        eval(parse(text=strThemeCall))
                        pListGlobal[[paste0('Plot',item,n)]]$pList[[1]]<<-pListGlobal[[paste0('TempPlot',item,n)]]
                      })
                      
                      # 
                      # 
                      # # DGP: What is this doing?
                      # #Render the updated plot
                      # observeEvent(input[["updateTheme",item,n]],{
                      #   output[[paste("Plot", item,n, sep="")]]<<-renderPlot({
                      #       do.call(pListPrint,update.Theme())
                      #     #pListGlobal[[paste0('Plot',item,n)]]
                      # })
                      # })
                      
                      # End of Theme Editor# ----
                      
                      
                      
                      # }else if(item=="ConcvTimeMult"){
                      #   output[[paste("Plot",item,n,sep="")]] <<- renderImage({ p1 },deleteFile=F)
                    }else{
                      # Probably one of demogTabCont, demogTabCat, NMTab, ConcvTimeMult
                      output[[paste("Plot",item,n,sep="")]]<<-renderImage(
                        renderTex(obj=pListGlobal[[paste('Plot',item,n)]],item=paste0(item,n),
                                  margin=c(left=input[[paste0("leftmargin",item,n)]],
                                           top=input[[paste0("topmargin",item,n)]],
                                           bottom=input[[paste0("bottommargin",item,n)]],
                                           right=input[[paste0("rightmargin",item,n)]]),
                                  footnote=input[[paste("Footnote",item,n,sep="")]])
                        ,deleteFile=F)
                    }
                    
                    if(debug){
                      input_nms <- names(input)
                      input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                      names(input_vals) <- input_nms
                      message <- "DEBUG E"
                      save(message,argList,input_vals,item,n,pListGlobal,file=file.path(srcDir,"tmp","message.rda"))
                    } 
                    
                    
                  } # End sameAsDefault!=1
                  
                }#end do any plots have input?
              }) #end local for this_n
              
            } #end if n!=0
            
          }) #end local for this_item
          
        }	 #end for this_item loop
      }) # end observer
    }) #end local
  }
  
  ##################################################
  
  observeEvent(input$outputGo,{
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " outputGo clicked"))
    
    for (this_item in plotList$type){
      isolate({
        
        local({
          item=this_item
          #Observe if any plots of that type have been assigned inputs
          if(length(grep(paste("reset", item, sep=""), names(input)))>0){
            
            numbers=as.numeric(unlist(str_extract_all(input[[paste(item, "Num", sep="")]], "\\d+")))
            if(length(numbers)>1){numRange=numbers[which(numbers!=0)]}
            if(length(numbers)==1){numRange=c(0:numbers)}
            if(length(numbers)==0){numRange=0}
            
            for (this_n in numRange){
              local({
                n=this_n
                if(n!=0){	
                  
                  if(input$saveAs!=""){
                    cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " outputGo for", item, n, 
                                                    ", outputGo value:", input$outputGo, 
                                                    ", inputButton value:", input[[paste("button",item,n,sep="")]]),"\n"))
                    
                    if(debug){
                      message <- "DEBUG OUTPUTGO"
                      input_nms <- names(input)
                      input_vals <- lapply(input_nms, function(inputi) try(input[[inputi]]))
                      names(input_vals) <- input_nms
                      save(message,item,n,input_vals,Defaults,file=file.path(debugDir,"message.rda"))
                    }
                    
                    Dir=sprintf("%s/%s_%s/", currentWD(), 
                                gsub("'","",
                                     gsub("[[:space:]]|\\.", "_", input$projectTitle)
                                ), 
                                Sys.Date()
                    )
                    dir.create(file.path(Dir,"PNG"),recursive=T)
                    
                    
                    argList=createArgList(input, item, n, dataFile=dataFile(), currentWD=currentWD())
                    callType=argList$callType
                    argList$callType=NULL
                    
                    # for(IDX in idx){
                    #   Defaults[[IDX]]<<-input[[IDX]]
                    # }
                    # Defaults[[paste("priorExists", item, n, sep="")]]<<-TRUE
                    
                    
                    if(item%in%"ConcvTimeMult"){
                      argList$tmpDir <- file.path(Dir,"PNG")
                      argList$regenPlots <- T
                    }
                    
                    #insert an error block around the plotting
                    p1 = tryCatch({
                      if(debug) save(callType,argList,file=file.path(debugDir,"output.rda"))
                      do.call(callType,args=argList)
                    }, 
                    error = function(e) {
                      if(debug) save(callType,argList,file=file.path(debugDir,"error.rda"))
                      arrangeGrob(textGrob(sprintf("You broke something\n%s", e)))
                    })
                    
                    # if(item %nin% c("demogTabCont","demogTabCat","NMTab")){
                    #   #Perform the actual plotting
                    #   output[[paste("Plot", item,n, sep="")]]<<-renderPlot({
                    #     print(p1)
                    #   })					
                    # }else if(item=="ConcvTimeMult"){
                    #   output[[paste("Plot",item,n,sep="")]] <<- renderImage({ p1 },deleteFile=F)
                    # }else{
                    #   output[[paste("Plot",item,n,sep="")]]<<-renderImage(
                    #     renderTex(obj=p1,item=paste0(item,n),
                    #               margin=c(left=input[[paste0("leftmargin",item,n)]],
                    #                        top=input[[paste0("topmargin",item,n)]],
                    #                        bottom=input[[paste0("bottommargin",item,n)]],
                    #                        right=input[[paste0("rightmargin",item,n)]]))
                    #     ,deleteFile=F)
                    # }
                    
                    #Create the save directory
                    
                    
                    
                    
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
                    
                    
                    p1List=
                      list(Facets=ifelse("facetBy" %in% names(argList), argList$facetBy, ""),
                           Marks=ifelse("markBy" %in% names(argList), argList$markBy, ""),
                           Groups=ifelse("groupBy" %in% names(argList), argList$groupBy, ""),
                           Stratification="",
                           LegendTitle=ifelse(paste("LegendTitle", item, n, sep="") %in% names(input), input[[paste("LegendTitle", item, n, sep="")]], ""),
                           Legend=ifelse(paste("Legend", item, n, sep="") %in% names(input), input[[paste("Legend", item, n, sep="")]], ""),
                           Footnote=ifelse(paste("Footnote",item,n,sep="") %in% names(input), input[[paste("Footnote",item,n,sep="")]], ""),
                           Plot=p1,
                           Type=plotList$sidebarType[plotList$type==item],
                           CSV=p1csv
                      )
                    
                    
                    # Make a docx version of the caption and footnote
                    p1List$docxCaption = renderTex(list(caption=p1List$Legend,
                                                        footnote=p1List$Footnote),
                                                   paste0(item,n), pandoc=T)
                    
                    p1Name=paste(item,n, sep="")
                    #if(callType=="ConcvTimeMult") p1List$Plot <- p1$src
                    
                    if(grepl("Exclusion",item)){
                      copy <- try(file.copy(from=p1$file,to=file.path(Dir,"PNG")))
                      if(class(copy)=="try-error"){
                        argList$tmpDir <- file.path(Dir,"PNG")
                        p1 <- do.call(callType,argList)
                        p1List$Plot <- p1
                      } 
                    }else if(item %in% c("inputTable", "inputListing", "inputFigure", "inputListing_text")){
                      if("src" %in% names(p1)){
                        p1List$Plot <- p1$src 
                        if(item=="inputTable") p1List$Footnote <- NULL
                      }else{
                        p1List$longText <- p1$preview
                      }
                    }else if(callType %nin% c("demogTabCont","demogTabCat","RNM")){
                      #savePlots(plotName=p1,  directory=Dir, saveName=paste(item,n, sep=""))
                      p1=do.call(callType,argList)
                      p1$fname=file.path(Dir,paste0(item,n))
                      do.call(pListSave,p1)
                    }else if(item%nin%"ConcvTimeMult"){
                      #argList$page=0
                      #p1=do.call(callType,argList)
                      #for(i in 1:length(p1)) pListSave(p1[[i]]$pList,plotCols = 1,plotRows = 1,fname = file.path(Dir,paste0(item,i)))
                      f <- renderTex(p1,item=item,footnote=p1List$Footnote,tmpDir=file.path(Dir,"PNG"),
                                     margin=c(left=10,top=5,right=50,bottom=5))
                      p1List$Plot <- f['src']
                    }
                    
                    
                    
                    # We only write the rda when using the TFL generated script.  
                    # guiGrobs object created here and stored in global env.
                    saveGrob(plot=p1List, Name=p1Name, file=sprintf("%s_Grobs.rda", fileHead),
                             writeRda=F)
                    
                    # Dump the captions and footnotes into the working directory
                    dir.create(file.path(Dir,"Captions"),recursive=T)
                    
                    for(nms in names(guiGrobs)){
                      if("docxCaption" %in% names(guiGrobs[[nms]])){
                        file.copy(guiGrobs[[nms]]$docxCaption,
                                  file.path(Dir,"Captions",basename(guiGrobs[[nms]]$docxCaption)))
                      }
                    }
                    
                    #Insert function recording into script	
                    #Save only the non-default arguments unless the user asks for a verbose script
                    
                    useArgs=argList
                    if(input$verbose){useArgs=createArgList(input,item,n,dataFile(),currentWD=currentWD(),complete=T)}
                    
                    #reduce argList to arguments used in the dataManip
                    argListManip=useArgs[names(useArgs) %in% names(formals(manipDat))]
                    
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
                    getObj <- function(x){
                      input_vals <- reactiveValuesToList(input)
                      titles <- lapply(grep("LegendTitle",names(input_vals)), function(xx) input_vals[[xx]])
                      names(titles) <- grep("LegendTitle",names(input_vals),value=T)
                      str_split(names(which(x == titles)),"LegendTitle")[[1]][2]
                    }
                    
                    figureOrder <- tryCatch(sapply(input[["figureOrder"]], getObj) ,
                                            error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","figureOrderError.rda")) )
                    tableOrder <- tryCatch(sapply(input[["tableOrder"]], getObj) ,
                                           error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","tableOrderError.rda")))
                    listingOrder <- tryCatch(sapply(input[["listingOrder"]], getObj),
                                             error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","ListingOrderError.rda")) )
                    
                    ordering <- c(tableOrder, figureOrder, listingOrder)
                    if(debug){
                      message="recordGUI"
                      input_vals <- reactiveValuesToList(input,all.names=T)
                      save(message, argList, item, callType, useArgs, input_vals, n,
                           argListManip, p1List=p1List, ordering,
                           file=file.path(srcDir,"tmp","recordGUI.rda"))
                    }
                    tryCatch(
                      recordGUI(doWhat=callType, 
                                toWhat=useArgs,
                                input=input,
                                item=item,
                                number=n,
                                manipArgs=argListManip,
                                currentWD=currentWD(),
                                grob=p1List,
                                ordering=ordering),
                      error=function(e){
                        cat(file=stderr(), sprintf("Record GUI failed with %s %s\n",item,n))
                        if(debug){
                          input_vals <- reactiveValuesToList(input,all.names=T)
                          save(e,callType, useArgs, input_vals, n, argListManip, file=file.path(srcDir,"tmp","record_output.rda"))
                        }
                      }
                    )
                  }
                }
              })
            }
          } # End if any input
        }) # End local
      }) # End for this item
    }
    
    observe(if(input$RTF & input$saveAs!="" ){
      withProgress(message = "Writing", value=.5,{
        isolate({
          cat(file=stderr(), paste0("LOG: ", Sys.time(), " writing RTF\n"))
          if(debug){
            message <- "writing RTF"
            input_nms <- isolate(names(input))
            input_vals <- isolate(lapply(input_nms, function(inputi) try(input[[inputi]])))
            names(input_vals) <- input_nms
            save(message,input_vals,guiGrobs,file=file.path(debugDir,"message.rda"))
          }
          ###############			
          #			make a document
          ###############	
          Dir=isolate(sprintf("%s/%s_%s/", currentWD(), gsub("[[:space:]]|\\.", "_", input$projectTitle), Sys.Date()))
          dir.create(Dir,showWarning=FALSE)
          fileHead=isolate(sprintf("%s%s_%s",Dir, input$saveAs, Sys.Date()))
          grobFile=sprintf("%s_Grobs.rda", fileHead)
          
          getObj <- function(x){
            input_vals <- reactiveValuesToList(input)
            titles <- lapply(grep("LegendTitle",names(input_vals)), function(xx) input_vals[[xx]])
            names(titles) <- grep("LegendTitle",names(input_vals),value=T)
            str_split(names(which(x == titles)),"LegendTitle")[[1]][2]
          }
          figureOrder <- tryCatch(sapply(input[["figureOrder"]], getObj) ,
                                  error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","figureOrderError.rda")) )
          tableOrder <- tryCatch(sapply(input[["tableOrder"]], getObj) ,
                                 error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","tableOrderError.rda")))
          listingOrder <- tryCatch(sapply(input[["listingOrder"]], getObj),
                                   error=function(e) if(debug) save(e,file=file.path(srcDir,"tmp","ListingOrderError.rda")) )
          ordering <- c(tableOrder, figureOrder, listingOrder)
          guiGrobs <- tryCatch(guiGrobs[ordering],
                               error=function(e) if(debug) save(e,ordering,file=file.path(srcDir,"tmp","guiGrobsOrderError.rda")))
          
          if(debug){
            save(guiGrobs,grobFile,ordering,file=file.path(srcDir,"tmp","rtferror.rda"))
          }
          tryCatch(writeRTF(grobFile, ordering=ordering),
                   error=function(e){
                     cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " failed to write RTF\n",e,"\n")))
                     # if(debug){
                     #   save(guiGrobs, grobFile, ordering, file=file.path(srcDir,"tmp","rtferror.rda"))
                     # }
                   })
          cat(file=stderr(), "LOG: Finished writing RTF\n")
        })
      })
    }) # End RTF observer
  }) # End observer for outputGo
  
  
  ##################################################
  
  observeEvent(input$newTemplateGo,{  
    cat(file=stderr(), paste0("LOG: ", Sys.time(), " newTemplateGo\n"))
    if(input$saveTemplateAs!=""){
      for(item in names(DefaultsFirst)){
        #put in the non-plot related Defaults too
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
               warning=function(w) cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " recordInput warning\n",w))),
               error=function(e)  cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " recordInput error\n",e)))
      )
    }
  })
  
  
  
  # Output and Saving Tabset  ----
  
  output$SaveTabset<-renderUI({
    cat(file=stderr(), paste(paste0("LOG: ", Sys.time(), " saveTabset\n")))
    wellPanel(
      #checkboxInput("PNG", "Record *.pngs", Defaults$PNG),
      checkboxInput("RTF", "Construct *.Doc", Defaults$RTF),
      checkboxInput("verbose", "Reveal Function Text (verbose R script)?", Defaults$verbose),
      textInput("saveAs", "File Name", Defaults$saveAs),
      actionButton("outputGo", "Save"),
      h1(),
      textInput("saveTemplateAs", "Template Name", Defaults$saveTemplateAs),
      actionButton("newTemplateGo", "Save")
      
    )
    
    
  })
  
  
  
  
  # Render Plot----  
  # output$Plot=renderPlot({
  #   pList.print(pList.new)
  # },height=1200)
  # observeEvent(input$updateElem,{
  #   output$Plot=renderPlot({
  #     if(input$sendElem==0){
  #       pList.print(pList.new)
  #     }else{
  #       #browser()
  #       pList.out=update.Layer()
  #       pList.print(pList.out)
  #     }
  #   },height=1200)
  # })
  # 
  # observeEvent(input$updateTheme,{
  #   output$Plot=renderPlot({
  #     if(input$sendTheme==0){
  #       pList.print(pList.new)
  #     }else{
  #       pList.out=update.Theme()
  #       pList.print(pList.out)
  #     }
  #   },height=1200)
  # })
  # 
  # observeEvent(input$SetThemeGrid,{
  #   pList.out=update.ThemeGrid()
  #   output$Plot=renderPlot({pList.print(pList.out)},height=1200)
  # })
  
  
  observeEvent(input$fastForward,{
    
    runjs("function sleep (time) {
                                    return new Promise((resolve) => setTimeout(resolve, time));
            };
            

            $('a[data-value=\"tabProjectInfo\"]').tab('show');
            
            sleep(2000).then(() => {$('a[data-value=\"Model Info\"]').tab('show');
                                      console.log('release 1');
                                    });
            
            sleep(3000).then(() => {
                                      $('a[data-value=\"tabDataInput\"]').tab('show');
                                      console.log('release 2');
                                    });
            
            sleep(5000).then(() => {
                                      $('#updateSourceView').click();
                                      console.log('release 3');
                                    });
            sleep(7000).then(() => {
                                     $('a[data-value=\"Run Data\"]').tab('show');
                                      console.log('release 4');
                                    });
            
            sleep(9000).then(() => {
                                      $('#updateRunView').click();
                                      console.log('release 5');
                                    });
            sleep(13000).then(() => {
                                      $('a[data-value=\"atabData\"]').tab('show');
                                      console.log('release 6');
                                    });
            sleep(14000).then(() => {
                                     $('#performMerge').click();
                                      console.log('release 7');
                                    });
            sleep(15000).then(() => {
                                      $('a[data-value=\"tabAnalysisSelection\"]').tab('show');
                                      console.log('release 8');
                                    });
            sleep(16000).then(() => {
                                      $('a[data-value=\"tabFigures\"]').tab('show');
                                      console.log('release 9');
                                    });
            sleep(17000).then(() => {
                                      $('#buttonConcvTime1').click();
                                      console.log('release 10');
                                    });
            ")
    
  })
  
  
  
  #End Shiny Server
})