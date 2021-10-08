# With the below options set, the strategy seems to be:
# 1: Restart your R session
# 2: Use 'shiny::runApp()' in the console. Let the app load, then shut it down.
# 3: Try to deploy using this script.
# 4: If this script fails, it will probably be because there are duplicate packages in the tflrepo. RSConnect will look for whatever isn't the
#    the latest package in an Archive folder because it likes to be pedantic. The Archive folder won't exist, so it will fail.
#.   In this case, go into /usr/local/repos/tfl/src/contrib and remove the unneeded packages, then use tools::write_PACKAGES("/usr/local/repos/tfl/src/contrib")
#.   to regenerate.
# 5: If you still get errors, particularly if you get errors asking for "digest", you'll have to keep troubleshooting. Sorry )=.

options(
  repos = c(
    # TFLCRAN = "https://metrumresearchgroup.github.io/tflcran/pkgs",
    # TFL = "file://~/tflpkgrepo",
    # MRAN =  "https://cran.microsoft.com/snapshot/2017-07-11"
    tflrepo = "file:///usr/local/repos/tfl"
    # mpn = "https://mpn.metworx.com/snapshots/stable/2021-09-11" # Needed for some deps that unforunately come with RSConnect.
  )
)

.libPaths(c("./rsconnect-deps/", .libPaths()))
# .libPaths(c(.libPaths(), "./rsconnect-deps/"))
library(curl) # Have to refresh this or RSC doesn't work.
rsconnect::deployApp(
  appName = "TFL",
  appDir = ".",
  contentCategory = "site",
  # forceUpdate = TRUE,
  appFiles = c(
    "server.R",
    "ui.R",
    "global.R",
    # list.files("tarball-pkgs", full.names = TRUE),
    # "tarball-pkgs/TFL_1.2.1.tar.gz",
    # "tarball-pkgs/GUI_1.2.1.tar.gz",
    paste0("NMStorage_uslv/", list.files("NMStorage_uslv", recursive = TRUE)),
    "packrat/packrat.opts"
  )
)
