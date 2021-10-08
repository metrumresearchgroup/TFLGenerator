options(
  repos = c(
    # TFLCRAN = "https://metrumresearchgroup.github.io/tflcran/pkgs",
    # TFL = "file://~/tflpkgrepo",
    # MRAN =  "https://cran.microsoft.com/snapshot/2017-07-11"
    tflrepo = "file:///usr/local/repos/tfl"
  )
)

.libPaths(c("./rsconnect-deps/", .libPaths()))
rsconnect::deployApp(
  appName = "TFL",
  appDir = ".",
  contentCategory = "site",
  forceUpdate = TRUE,
  appFiles = c(
    "server.R",
    "ui.R",
    "global.R",
    # list.files("tarball-pkgs", full.names = TRUE),
    "tarball-pkgs/TFL_1.2.1.tar.gz",
    "tarball-pkgs/GUI_1.2.1.tar.gz",
    paste0("NMStorage_uslv/", list.files("NMStorage_uslv", recursive = TRUE)),
    "packrat/packrat.opts"
  )
)
