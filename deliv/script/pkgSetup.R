# Expects to be run from script
rm(list=ls())
author <- c("danp","polhamus")
pkgs <- c('metrumrg','shinyFiles','animation','gdata','shinydashboard',
          'DT','rtf','lazyeval','dplyr','DBI','ggplot2','gridExtra',
          'readr','shinyAce','shinyBS','TFL','GUI','colourpicker','shinyjs')

# Create the shiny path and touch some files there so the Makefile can run
if(dir.exists("/data/shiny-server") & file.exists("../Makefile")){
  # Then I'm assuming you're set up correctly.  If you are just setting up the packages and not installing
  # the TFL generator, then this block should not run.
    dir.create("/data/shiny-server/TFL generator")
    system("touch /data/shiny-server/TFL\\ generator/server.R")
    system("touch /data/shiny-server/TFL\\ generator/ui.R")
    system("touch /data/shiny-server/TFL\\ generator/restart.txt")
    od <- getwd()
    setwd("..")
    system("make update_server signal_restart")
    setwd(od)
}

user <- Sys.info()["user"]
parentScriptDir <- getwd()  ## You may need to mod this to be in the top level of scriptDir
if(!exists("pkgDir")) pkgDir <- file.path(parentScriptDir, "pkg")
libDir <- file.path(parentScriptDir, "lib")
dir.create(libDir)
dir.create(pkgDir)

.libPaths(libDir)
mycran <- paste("file://", pkgDir, sep="")
library(tools)
if(file.exists(file.path(pkgDir,"PACKAGES"))){
  available <- available.packages(contriburl=mycran)[,"Package"]
}else{
  write_PACKAGES(pkgDir)
  available <- available.packages(contriburl=mycran)[,"Package"]
}


## Only authors can install from CRAN and write_PACKAGES
fromCRAN <- F
# fromCRAN <- user %in% author
# if(fromCRAN){
#   newpkgs <- setdiff(pkgs, available)
#   write_PACKAGES(pkgDir)
#   if("audited" %in% newpkgs){
#     download.file("https://metrumrg-soft.s3.amazonaws.com/audited/audited_1.9.tar.gz",destfile=file.path(pkgDir,"audited_1.9.tar.gz"))
#     write_PACKAGES(pkgDir)
#   }
#   if("fork" %in% newpkgs){
#     download.file("https://metrumrg-soft.s3.amazonaws.com/fork/fork_1.2.4.tar.gz",destfile=file.path(pkgDir,"fork_1.2.4.tar.gz"))
#     write_PACKAGES(pkgDir)
#   }
#   if("review" %in% newpkgs){
#     download.file("https://metrumrg-soft.s3.amazonaws.com/review/review_2.5.tar.gz",destfile=file.path(pkgDir,"review_2.5.tar.gz"))
#     write_PACKAGES(pkgDir)
#   }
#   if("metrumrg" %in% newpkgs){
#     download.file("http://download.r-forge.r-project.org/src/contrib/metrumrg_5.57.tar.gz", destfile="pkg/metrumrg_5.57.tar.gz")
#     write_PACKAGES(pkgDir)
#   }
#   if(length(newpkgs)>0){
#     install.packages(newpkgs,
#                      lib=libDir,
#                      contriburl=c(mycran,
#                                   contrib.url("http://r-forge.r-project.org","source"),
#                                   contrib.url("https://cran.rstudio.com/","source")),
#                      destdir=pkgDir,
#                      type="source",
#                      INSTALL_opts="--no-multiarch")
#     write_PACKAGES(pkgDir)
#   }
#   ## If multiple authors qcing each other, a package could be available but uninstalled.  Install from local.
#   uninstalled <- setdiff(pkgs, installed.packages(libDir))
#   if(length(uninstalled)>0){
#     install.packages(uninstalled,
#                      lib = libDir,
#                      contriburl = mycran,
#                      type = "source",
#                      INSTALL_opts="--no-multiarch")
#   }    
# }
if(!fromCRAN){
  installed <- row.names(installed.packages(libDir))
  newpkgs <- setdiff(pkgs, installed)
  if(length(newpkgs)>0){
    if("metrumrg" %in% newpkgs){
      # XML frequently fails to build from source.  Use the binary and place it in lib.
      if(!"XML" %in% rownames(installed.packages())) install.packages("XML",lib=libDir,repos="https://cran.rstudio.com/",INSTALL_opts="--no-multiarch",destdir=pkgDir)
      install.packages('metrumrg', repos=c('http://R-Forge.R-project.org','https://cran.rstudio.org'))
    }
    
    install.packages(newpkgs,
                     lib = libDir,
                     contriburl = mycran,
                     type = "source",
                     INSTALL_opts="--no-multiarch")
  }
}



