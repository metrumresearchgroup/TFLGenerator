if(file.exists("renv/activate.R")){
  source("renv/activate.R")
}
if(interactive()){
  message("repos set to: \n\t", paste0(getOption('repos'), collapse = "\n\t"))
  message("library paths set to: \n\t", paste0(.libPaths(), collapse = "\n\t"))
}
options(
  repos = c(
    # TFLCRAN = "https://metrumresearchgroup.github.io/tflcran/pkgs",
    TFL = "file://~/tflpkgrepo",
    MRAN =  "https://cran.microsoft.com/snapshot/2017-07-11"
  )
)
