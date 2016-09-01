#rm(list=ls(all=TRUE))
.libPaths("/data/tflgenerator/script/lib")
library(GUI)
library(TFL) # the Amgen Internal TFL package
library(shiny)
library(shinydashboard)
library(shinyFiles)

# Defaults are used to keep the current entries in dynamic memory, 
#   defaults first are the pre-defined defaults

# Define UI fordataset viewer application

	dashboardPage(
      	# Application title
      	dashboardHeader(title="Pharmacometrics TFL"),
      	dashboardSidebar(
      	  sidebarMenu(
      	     
      	      # selectInput(inputId="templateSelection", label="Open Template", choices=c("New Analysis"="Template_New","Template_V8"="Template_V8", "Template_Other"="Template_Other")),
      	    selectInput(inputId="templateSelection", label="Open Template", choices=c("New Analysis"="Template_New")),
      	    
      	      fileInput(inputId="templatePath", label="Select Personal Template", multiple=FALSE),
      	      h1(""),
      	      actionButton("templateGo", "Go"),
      	      h1(""),
      	      # shinyDirButton(id="dataPath", label='Set working directory', title='Please select a working directory'),
      	      h1(""),
      	      menuItem(text="Pharmacometrics TFL", tabName = "tabIntro", icon=icon("clock")),
         	    menuItem(text="Project info", tabName="tabProjectInfo", icon=icon("sliders")),
      	      menuItem(text="Data input", tabName="tabDataInput", icon=icon("sliders")),
      	      menuItem(text="Analysis data", tabName="atabData", icon=icon("database")),
      	      menuItem(text="Data Exclusions", tabName="tabDataExc", icon=icon("sliders")),
      	      menuItem(text="Analysis Selection", tabName="tabAnalysisSelection", icon=icon("check-square-o")),
      	      menuItem(text="Figures", tabName="tabFigures", icon=icon("bar-chart")),
      	      menuItem(text="Tables", tabName="tabTables", icon=icon("table")),
      	      menuItem(text="Listings", tabName="tabListings", icon=icon("navicon")),
      	      menuItem(text="Current TFL", tabName="tabCurrentTFL", icon=icon("file-word-o")),
      	      menuItem(text="Save and Export", tabName="tabOutput", icon=icon("mail-forward"))
      	      
      	  )
      	),
      	dashboardBody(
      	  tabItems(
      	    tabItem(tabName="tabIntro",
      	            h2("User guide:",
      	               a("Link",href="https://docs.google.com/document/d/1sGwrTt_rr2gX0X8Ifgaj1VcFWZkY4-Xo4FF4flzWkt8/edit?usp=sharing")),
      	            h1(""),
      	            tags$iframe(src="https://docs.google.com/document/d/1sGwrTt_rr2gX0X8Ifgaj1VcFWZkY4-Xo4FF4flzWkt8/pub?embedded=true",
      	                        height="600px", width="100%")
      	            # ,
      	            # h1(""),
      	            # h2("Source code:"),
      	            # verbatimTextOutput("sourceParseCode")
      	            # ,
      	            # h1(""),
      	            # h2("Input names:"),
      	            # code(verbatimTextOutput("readThis"))
      	            # ,
      	            # h1(""),
      	            # h2("Client data:"),
      	            # verbatimTextOutput("clientDataText")
      	            # ,
      	            # h1(""),
      	            # h2("Here's a table rendering:"),
      	            # imageOutput("texTable")
      	    ),
      	    tabItem(tabName="tabProjectInfo", 
      	            h3("Project info"), 
      	            uiOutput("projectInfoTabset")
      	    ),
      	    tabItem(tabName="tabDataInput",
      	            h3("Data input and manipulation"),
      	            uiOutput("DataTabset")
      	     ),
      	    tabItem(tabName="atabData",
      	            h3("Analysis data merge specification and manipulation"),
      	            uiOutput("aDataTabset")
      	    ),      	    
      	    tabItem(tabName="tabDataExc",
      	            h3("Data exclusions"),
      	            uiOutput("DataExcTabset")
      	    ),
      	    tabItem(tabName="tabData",
      	            h3("Data"),
      	            uiOutput("outputTabset")
      	    ),
      	    tabItem(tabName="tabAnalysisSelection",
      	            h3("Analysis Selection"),
      	            #checkboxInput(inputId="Color", label="Color?", value=TRUE),
      	            uiOutput("PlotTabset")
      	    ),
      	    tabItem(tabName="tabFigures",
      	            h3("Figures"),
      	            uiOutput("figuresTabset")
      	    ),
      	    tabItem(tabName="tabTables",
      	            h3("Tables"),
      	            uiOutput("tablesTabset")
      	    ),
      	    tabItem(tabName="tabListings",
      	            h3("Listings"),
      	            uiOutput("listingsTabset")
      	    ),
      	    tabItem(tabName="tabCurrentTFL",
      	            h3("Current TFL"),
      	            uiOutput("currentTFLTabset")
      	    ),
      	    tabItem(tabName="tabOutput",
      	            h3("Output"),
      	            uiOutput("SaveTabset")
      	    )
      	  )
      	)
	)