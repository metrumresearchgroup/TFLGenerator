#' This must be run first to force an old installation of gridExtra and ggplot2, and run as root
#' This will install into /usr/local/lib/R/site-library as it appears before /usr/lib/R/library (.Library)
#' on the site PATH.  Shiny should find these older instantiations in /usr/local/lib/R/site-library first.
system("apt-get install -y imagemagick")
author <- c("root")

dir.create("/root/Rpkg")
download.file("https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_1.0.1.tar.gz",
              destfile="/root/Rpkg/ggplot2_1.0.1.tar.gz")
download.file("https://cran.r-project.org/src/contrib/Archive/gridExtra/gridExtra_0.9.tar.gz",
              destfile=file.path("/root/Rpkg/gridExtra_0.9.tar.gz"))
download.file("https://cran.r-project.org/src/contrib/Archive/Hmisc/Hmisc_3.17-0.tar.gz",
              destfile=file.path("/root/Rpkg/Hmisc_3.17-0.tar.gz"))

library(tools)
write_PACKAGES("/root/Rpkg")
install.packages(c("ggplot2","gridExtra","Hmisc"),
                 lib = .libPaths()[1],
                 contriburl = c(paste("file://", "root/Rpkg", sep="")),
                 type = "source",
                 INSTALL_opts="--no-multiarch")

system("restart shiny-server")
