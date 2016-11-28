f <- list.files("screencaps")

figs <- vector(mode="list",length=length(f))
figs <- list()
for(fi in f){
    fis <- strsplit(fi,"-")[[1]][1:2]
    figs <- c(figs,sprintf(
        "\\begin{figure}[H]\n\\includegraphics[width=.8\\textwidth]{screencaps/%s}\n\\caption{RID: %s Test ID: %s}\n\\end{figure}",
        fi,
        fis[1],
        fis[2]
        ))
}
write(unlist(figs),file="include.tex")
