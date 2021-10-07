
# Order matters
# tarballs <- c(
#   # "rappdirs_0.3.1.tar.gz",
#   # "reticulate_1.14.tar.gz",
#   # "rtf_0.4-11.tar.gz",
#   # "XML_3.98-1.9.tar.gz",
#   # "metrumrg_5.57.tar.gz",
#   "TFL_1.2.1.tar.gz",
#   "GUI_1.2.1.tar.gz"
# )
# 

# 
# pkgs <- strsplit(tarballs, "_")
# 
# # remove.packages(unlist(lapply(pkgs, function(x){x[1]})))
# 
# for(i in 1:length(pkgs)){
#   
#   if(!requireNamespace(pkgs[[i]][1], quietly = TRUE)){
#     install.packages(
#       file.path("tarball-pkgs", paste(pkgs[[i]], collapse = "_")),
#       repos = NULL, 
#       type="source"
#     )
#   }
#   
#   
# }
# 


