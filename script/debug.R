# This file is simply here to help us troubleshoot deployment issues.
rsconnect::writeManifest(
  appFiles = c(
    "server.R",
    "ui.R",
    "global.R",
    # list.files("tarball-pkgs", full.names = TRUE),
    "tarball-pkgs/TFL_1.2.1.tar.gz",
    "tarball-pkgs/GUI_1.2.1.tar.gz",
    paste0("NMStorage_uslv/", list.files("NMStorage_uslv", recursive = TRUE)),
    "packrat/packrat.opts"
  ),
  appDir = "."
)
