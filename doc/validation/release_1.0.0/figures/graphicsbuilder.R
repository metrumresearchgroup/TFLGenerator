setwd(file.path(getwd(),'doc/validation/release_1.2.0/figures/'))
f <- list.files("screencaps")

figs <- vector(mode="list",length=length(f))
figs <- list()
for(fi in f){
    fis <- gsub('[a-z.]','',strsplit(fi,"-")[[1]][1:3])
    figs <- c(figs,sprintf(
        "\\begin{figure}[H]\n\\includegraphics[width=.8\\textwidth]{screencaps/%s}\n\\caption{RID: %s Test ID: %s IDNum: %s}\n\\end{figure}",
        fi,
        fis[1],
        fis[2],
        fis[3]
        ))
}
write(unlist(figs),file="include.tex")
