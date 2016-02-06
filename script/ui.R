#rm(list=ls(all=TRUE))
.libPaths("/data/tflgenerator-ge0.9/script/lib")
library(ggplot2,lib="/usr/local/lib/R/site-library")
library(gridExtra,lib="/usr/local/lib/R/site-library")
library(GUI)
library(TFL) # the Amgen Internal TFL package
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(DT)

#Defaults are used to keep the current entries in dynamic memory, defaults first are the pre-defined defaults


# Define UI fordataset viewer application

	dashboardPage(
      	# Application title
      	dashboardHeader(title="AMGEN TFL Generator"),
      	dashboardSidebar(
      	  sidebarMenu(
      	     
      	      selectInput(inputId="templateSelection", label="Open Template", choices=c("New Analysis"="Template_New","Template_V8"="Template_V8", "Template_Other"="Template_Other")),
      	      fileInput(inputId="templatePath", label="Select Personal Template", multiple=FALSE),
      	      h1(""),
      	      actionButton("templateGo", "Go"),
      	      h2(""),
      	      menuItem(text="The Amgen TFL Generator", tabName = "tabIntro", icon=icon("clock")),
      	      menuItem(text="Data Input", tabName="tabDataInput", icon=icon("sliders")),
      	      menuItem(text="Data", tabName="tabData", icon=icon("database")),
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
      	            h3("Intro, help, and link to how-to files")
      	    ),
      	    tabItem(tabName="tabDataInput",
      	            h3("Data Input"),
      	            uiOutput("DataTabset")
      	     ),
      	    tabItem(tabName="tabData",
      	            h3("Data"),
      	            uiOutput("outputTabset")
      	    ),
      	    tabItem(tabName="tabAnalysisSelection",
      	            h3("Analysis Selection"),
      	            checkboxInput(inputId="Color", label="Color?", value=TRUE),
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
      	            h3("Current TFL")
      	    ),
      	    tabItem(tabName="tabOutput",
      	            h3("Output"),
      	            uiOutput("SaveTabset")
      	    )
      	  )
      	)
	)