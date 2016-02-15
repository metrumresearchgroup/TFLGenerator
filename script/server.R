
#rm(list=ls(all=TRUE))
srcDir <- "/data/tflgenerator-ge0.9"
debug <- TRUE

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  Defaults<<-DefaultsFirst
  unlockBinding("tabList", as.environment("package:GUI"))
  tabList<<-tabList()
  plotList$sidebarType <- c("Figures","Figures","Tables","Figures","Figures","Figures","Figures","Figures","Figures","Tables","Figures","Tables")
  
  #Open Template
  
  
  observeEvent(input$templateGo,{
    inFile <- input$templatePath
    if (is.null(inFile))
      return(NULL)
    source(inFile$datapath)
  }) 
  
 observe(if(TRUE){  	
  inFile <- input$templatePath
  if (is.null(inFile))
    return(NULL)
  source(inFile$datapath)
 })
  
  ############
  #Setting Color Schemes
  ############
  
  unlockBinding("cleanScales", as.environment("package:TFL"))
  
  observe(
    if("Color" %in% names(input)){
      if(input$Color){
        cleanScales<<-setColorScale()}
    }
  )
  
  observe(
    if("Color" %in% names(input)){
      if(!input$Color){
        cleanScales<<-setGrayScale()}
    }
  )
  

  
  ##############	
  #Initial/external interactions
  ##############
  
  #read data in a reactive format
  dataFile=reactive({

    if(input$dataPath %in% c("Defaults to Working Directory", "", " ")){
      currentWD<<-workingDirectory
    }
    
    
    if(input$dataPath %nin% c("Defaults to Working Directory", "", " ")){
      currentWD<<-input$dataPath
    }
    
    if(input$runno=="#"){
      return()
    }
    
    
    extensions=unlist(str_split(input$ext, ","))
    extensions=gsub("[[:space:]]*", "", extensions)
    
    runs=input$runno
    runs=unlist(str_split(input$runno, ","))
    runs=gsub("[[:space:]]*", "", runs)
    
    dat=matrix()
    for(irun in runs) {
          for(iext in extensions){
            ext=iext
            fileName=sprintf("%s/%s/%s%s", currentWD, irun, irun, ext)
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
    
    dat=data.frame(dat, stringsAsFactors=TRUE)
    names(dat)[which(names(dat)==input[["DVCol"]])]="DV"
    names(dat)[which(names(dat)==input[["TAFDCol"]])]="TAFD"
    names(dat)[which(names(dat)==input[["STUDCol"]])]="STUD"
    names(dat)[which(names(dat)==input[["NMIDCol"]])]="NMID"
    
    #rename columns
    rename=ifelse("renameThese" %in% names(input), input[["renameThese"]], "")
    if(nchar(rename)>1){
      rename=gsub("\n$", "", rename)
      rename=unlist(str_split(rename, "\n"))
      rename=str_split(rename, ";[[:space:]]*")
      for(i in length(rename)){
        if(nchar(rename[[i]][1])>1){
          names(dat)[which(names(dat)==rename[[i]][1])]=rename[[i]][2]
        }	
      }
    }
    
    
    
    #factor entire sets
    fF=ifelse("factorThese" %in% names(input), input[["factorThese"]], "")
    if(nchar(fF)>1){
      fF=gsub("\n$", "", fF)
      fF=unlist(str_split(fF, "\n"))
      fF=str_split(fF, ";[[:space:]]*")
      for(i in length(fF)){
        if(nchar(fF[[i]][1])>1){
          dat[,fF[[i]][1]]=factor(dat[,fF[[i]][1]], levels=unlist(str_split(fF[[i]][2], ",[[:space:]]*")), labels=unlist(str_split(fF[[i]][3], ",[[:space:]]*")))	
        }	
      }
    }
    
    #perform limits and transformations
    #Deal with formatting the data limits for functions
    wholeLim=limitations(input[["dataLimits"]])	
    wholeTrans=transformations(input[["dataTrans"]])
    dat=manipDat(dat, dataLimits=wholeLim, dataTrans=wholeTrans)
    
    # For debugging, save a copy of input
    if(debug){
      dati <- dat
      tabList <- get("tabList",envir=.GlobalEnv)
      save(dati,file=file.path(srcDir,"tmp","shinytmpdat.rda"))
    }
    # End debugging

    return(dat)
  })	
  
  
  ##############	
  # Output Renders - Just the data set overview and plot title
  ##############
 

  
   
  #Raw Contents
  output$contentsHead <- DT::renderDataTable({
    if("runno" %in% names(input)){	
      if (input$runno=="#") {
        return()
      }
      if(is.null(dataFile())){
        return()
      }
    }
    return(head(dataFile(), n = 20))
  })
  
  
 
  
  #Raw Contents
  output$contentsSummary <- DT::renderDataTable({
    if("runno" %in% names(input)){	
      if (input$runno=="#") {
        return()
      }
      if(is.null(dataFile())){
        return()
      }
      
      fakeData=dataFile()
      fakeData=suppressWarnings(data.frame(apply(fakeData, c(1,2), as.numeric)))
      fakeData=data.frame(summary(fakeData))
      fakeData$Var2=as.character(fakeData$Var2)
      fakeData$Var1=str_extract(fakeData$Freq, ".*:")
      fakeData$Freq=gsub(".*:","", fakeData$Freq)
      fakeData=fakeData[!is.na(fakeData$Var1),]
      
      fakeData=data.frame(cast(fakeData, Var1 ~ Var2, value="Freq"), check.names=FALSE, stringsAsFactors=FALSE)
      
      row.names(fakeData)=fakeData$Var1
      fakeData=data.frame(fakeData[order(row.names(fakeData), decreasing=TRUE),], check.names=FALSE, stringsAsFactors=FALSE)
      fakeData$Var1=NULL
      names(fakeData)=gsub("^[[:space:]]*", "", names(fakeData))
      fakeData=fakeData[,names(dataFile())]
      return(fakeData)
    }
  })
  
  output$contentsStat <- DT::renderDataTable({
    if("runno" %in% names(input)){	
      if (input$runno=="#") {
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
    #This prints out the input project title to the appropriate GUI location
    input$projectTitle
  })
  
  
#Data Input Tabset
  
  output$DataTabset <- renderUI({
    tabsetPanel(
             tabPanel(title="Project Information",
                      wellPanel(
                        textInput(inputId="projectTitle", label="Project Title:", value=Defaults$projectTitle),
                        boxInputLarge(inputId="projectInfo", label="Project Information:", value=Defaults$projectInfo)
                        )
                      ),
             tabPanel(title="Model Info",
                      wellPanel(
                        textInput(inputId="dataPath", label="Data Path:", value=Defaults$dataPath),	
                        textInput(inputId="numModel", label="Number of Models", value="1"),
                        textInput(inputId="runno", label="Run Numbers:", value=Defaults$runno),
                        textInput(inputId="ext", label="File Extension:", value=Defaults$ext),
                        checkboxInput('header', 'Header?', value=Defaults$header),
                        numericInput("skipLines", "Skip Lines:", value=Defaults$skipLines),
                        textInput(inputId="baseModel", label="Base Model #", value="")
                      )
              ),
             tabPanel(title="Change E-R SSAP Defaults",
                      wellPanel( textInput("DVCol", "DV Column", Defaults$DVCol),
                                 textInput("TAFDCol", "TAFD Column", Defaults$TAFDCol),
                                 textInput("STUDCol", "STUD Column", Defaults$STUDCol),
                                 textInput("NMIDCol", "NMID Column", Defaults$NMIDCol)
                                
                      )
             ), 
             tabPanel(title="Modify All Data",
                    wellPanel(
                       boxInputLarge(inputId="renameThese", "Rename Columns", value=Defaults$renameThese),
                      boxInputLarge(inputId="factorThese", "Factor Columns", value=Defaults$factorThese),
                      boxInputLarge(inputId="dataLimits", "Limit Data by", value=Defaults$dataLimits),
                      boxInputLarge(inputId="dataTrans", "Transform Data:", value=Defaults$dataTrans) 
                      )
    )

             
    )
  })

#Data Tabset  
  output$outputTabset <- renderUI({		
    #The first PanelSet is what is loaded with the base defaults.  
    PanelSet=list(tabPanel("Raw Data",
                           fluidRow(
                             column(width = 12,
                                    box(
                                      title = "", width = NULL, status = "primary",
                                      div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsHead'))
                                    )
                             )
                           )
                            ),
                   tabPanel("Data Summary", 
                            fluidRow(
                              column(width = 12,
                                     box(
                                       title = "Summary", width = NULL, status = "primary",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsSummary'))
                                     )
                              )
                            ),
                            fluidRow(
                              column(width = 12,
                                     box(
                                       title = "Statistics", width = NULL, status = "primary",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput('contentsStat'))
                                     )
                              )
                            )

                            )
                  )
    dummy=(do.call(tabsetPanel, PanelSet))
    return(dummy)
    

  })
  

# Analysis Selection
  output$PlotTabset<-renderUI({
  
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
   
    # plotList$sidebarType=c("Figures","Figures","Tables","Figures","Figures","Figures","Figures","Figures","Figures","Figures", "Tables", "Tables")
    type="Figures"
    types=grep(type, plotList$sidebarType)
    PanelSet=list()
    for (item in plotList$type[types]){
      if(paste(item, "Num", sep="") %in% names(input)){
        PanelSet=do.call(what=plotList$Call[plotList$type==item], args=list(plotType=item, input=input, Set=PanelSet) )
      }
    }
    return(do.call(tabsetPanel, PanelSet))

  })
  output$listingsTabset<-renderUI({
    
    # plotList$sidebarType=c("Figures","Figures","Tables","Figures","Figures","Figures","Figures","Figures","Figures","Figures", "Tables", "Tables")
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
    # plotList$sidebarType=c("Figures","Figures","Tables","Figures","Figures","Figures","Figures","Figures","Figures","Figures", "Tables", "Tables")
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

  
  ############
  #Generating Plots, internal and saving
  ############
  
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
                #check if the defaults/inputs for a plot have been created
                if(length(grep(paste(item, n,sep=""), names(input)))>0){
                  
                  ##Check to see if the input has changed since the last time it was rendered
                  idx=grep(paste(item,n,sep=""), names(Defaults), value=TRUE)
                  idn=grep(paste(item,n,sep=""), names(input), value=TRUE)
                  
                  #some values don't exist as inputs, for example the priors variable to check for priors
                  idtest=idx[idx %in% idn]
                  
                  #because of the reactive nature of 'input' comparisons have to be done one at a time
                  sameAsDefault=sum(sapply(idtest, function(X){input[[X]]==Defaults[X]}))/length(idtest)
                  
                  if(sameAsDefault!=1){
                    
                    argList=createArgList(input, item, n, dataFile=dataFile())
                    callType=argList$callType
                    argList$callType=NULL
                    
                    
                    if(is.null(dataFile())){
                      return()
                    }
                    
                    for(IDX in idx){
                      Defaults[IDX]<<-input[[IDX]]
                    }
                    Defaults[paste("priorExists", item, n, sep="")]<<-TRUE
                    
                    #insert an error block around the plotting
                    p1 = tryCatch({
                      if(debug) save(callType,argList,file=file.path(srcDir,"tmp","output.rda"))
                      do.call(callType,args=argList)
                    }, warning = function(w) {
                      arrangeGrob(textGrob(sprintf("You broke something\n%s", w)),
                                  do.call(callType,args=argList),
                                  heights=c(0.05,1))
                    }, error = function(e) {
                      if(debug) save(callType,argList,file=file.path(srcDir,"tmp","error.rda"))
                      arrangeGrob(textGrob(sprintf("You broke something\n%s", e)))
                    }
                    )
                    
                    #Perform the actual plotting
                    output[[paste("Plot", item,n, sep="")]]<<-renderPlot({
                      print(p1)
                    })					
                    
                  }
                  
                  observeEvent(input$outputGo,{
                    if(input$saveAs!=""){
                      argList=createArgList(input, item, n, dataFile=dataFile())
                      callType=argList$callType
                      argList$callType=NULL
                      
                      
                      if(is.null(dataFile())){
                        return()
                      }
                      
                      for(IDX in idx){
                        Defaults[IDX]<<-input[[IDX]]
                      }
                      Defaults[paste("priorExists", item, n, sep="")]<<-TRUE
                      
                      #insert an error block around the plotting
                      p1 = tryCatch({
                        if(debug) save(callType,argList,file=file.path(srcDir,"tmp","output.rda"))
                        do.call(callType,args=argList)
                      }, warning = function(w) {
                        arrangeGrob(textGrob(sprintf("You broke something\n%s", w)),
                                    do.call(callType,args=argList),
                                    heights=c(0.05,1))
                      }, error = function(e) {
                        if(debug) save(callType,argList,file=file.path(srcDir,"tmp","error.rda"))
                        arrangeGrob(textGrob(sprintf("You broke something\n%s", e)))
                      })
                      
                      #Perform the actual plotting
                      output[[paste("Plot", item,n, sep="")]]<<-renderPlot({
                        print(p1)
                      }
                      )					
                      
                      
                      #Create the save directory
                      
                      Dir=sprintf("%s/%s/%s_%s/", currentWD, input$runno, gsub("[[:space:]]|\\.", "_", input$projectTitle), Sys.Date())
                      
                      
                      dir.create(Dir,showWarning=FALSE)
                      fileHead=sprintf("%s%s_%s",Dir, input$saveAs, Sys.Date())
                      cat(Dir)
                      
                      if(debug) save(Dir,fileHead,file=file.path(srcDir,"tmp","savedir.rda"))
                      ###############
                      #			Save plots and grobs, record the script
                      ################			
                      
                      p1=do.call(callType, 
                                 args=argList
                      )
                      
                      p1csv=data.frame(1)
                      #correct for tables
                      if(callType %in% c("listofCallTypesTables")){
                        p1csv=p1
                        p1=tableGrob()					
                      }
                      
                      
                      p1List=list(Facets=ifelse("facetBy" %in% names(argList), argList$facetBy, ""),
                                  Marks=ifelse("markBy" %in% names(argList), argList$markBy, ""),
                                  Groups=ifelse("groupBy" %in% names(argList), argList$groupBy, ""),
                                  Stratification="",
                                  LegendTitle=ifelse(paste("LegendTitle", item, n, sep="") %in% names(input), input[[paste("LegendTitle", item, n, sep="")]], ""),
                                  Legend=ifelse(paste("Legend", item, n, sep="") %in% names(input), input[[paste("Legend", item, n, sep="")]], ""),
                                  Plot=p1,
                                  CSV=p1csv
                      )
                      p1Name=paste(item,n, sep="")
                      if(input$PNG){
                        savePlots(plotName=p1,  directory=Dir, saveName=paste(item,n, sep=""))
                      }
                      saveGrob(plot=p1List, Name=p1Name, file=sprintf("%s_Grobs.rda", fileHead))
                      
                      #Insert function recording into script	
                      #Save only the non-default arguments unless the user asks for a verbose script
                      
                      useArgs=argList
                      if(input$verbose){useArgs=argListComplete}
                      
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
                      
                      #Keep a recording of the complete list <-this is only used to write out the complete argument list in the verbose script
                      argListComplete=formals(callType)
                      argListComplete$"..."=NULL
                      for(listName in names(argList)){
                        argListComplete[[listName]]=argList[[listName]]
                      }
                      
                      recordGUI(doWhat=callType, 
                                toWhat=useArgs,
                                input=input,
                                number=n,
                                manipArgs=argListManip
                      )
                      
                      
                    }	
                  })     
                  
                  
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
  
  observeEvent(input$outputGo,{  
      observe(if(input$RTF & input$saveAs!="" ){
        
        ###############			
        #			make a document
        ###############	
        Dir=sprintf("%s/%s/%s_%s/", currentWD, input$runno, gsub("[[:space:]]|\\.", "_", input$projectTitle), Sys.Date())
        dir.create(Dir,showWarning=FALSE)
        fileHead=sprintf("%s%s_%s",Dir, input$saveAs, Sys.Date())
        grobFile=sprintf("%s_Grobs.R", fileHead)
        writeRTF(grobFile)
      })  
  })
  
  observeEvent(input$newTemplateGo,{  
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
             "recall"))
             {Defaults[[item]]<<-input[[item]]}
         }
         recordInput(input=input,Defaults=Defaults)
     }
       })

  
  #Output and Saving Tabset  
  
  output$SaveTabset<-renderUI({
    
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
