rsconnect::deployApp(
  appName = "TFL",
  appDir = ".",
  contentCategory = "site",
  
  appFiles = c(
    "server.R",
    "ui.R",
    "global.R",
    "GUI_1.2.1.tar.gz",
    paste0("NMStorage_uslv/", list.files("NMStorage_uslv", recursive = TRUE)),
    "packrat/packrat.opts"
  )
)
