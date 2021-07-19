options(
  repos = c(
    # TFLCRAN = "https://metrumresearchgroup.github.io/tflcran/pkgs",
    # MRAN =  "https://cran.microsoft.com/snapshot/2020-03-18"
    LOCALTFL = "file:///usr/local/repos/tfl"
  )
)

if(file.exists("renv/activate.R")){
  source("renv/activate.R")
}
if(interactive()){
  message("repos set to: \n\t", paste0(getOption('repos'), collapse = "\n\t"))
  message("library paths set to: \n\t", paste0(.libPaths(), collapse = "\n\t"))
}
