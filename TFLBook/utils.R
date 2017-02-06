help.chunk=function(fn,output){
if(!is.character(fn)) fn=as.character(substitute(fn))

header=paste0("## ",fn)

chnk1=
paste0(
"```{r results='asis',echo=FALSE}
fn.src('",fn,"',hlp.dir)
```")

if(output=='figure'){
  chnk2=paste0("```{r ",fn,", echo=FALSE,fig.show='hide',warning=FALSE}
example(",fn,",ask = F)")
  
  chnk3=
    paste0("lapply(list.files(fig.dir,pattern = '",fn,"-',full.names = T),
           function(x) base64enc::dataURI(file = x, mime='image/png'))%>%
           loryR(images_per_page = 1, options = list(rewind=TRUE))
```
")}
  
if(output=='table'){
chnk2=paste0(
"```{r ",fn,", echo=FALSE}
ex.out=example(",fn,",echo = F)$value
if(!is.list(ex.out)) ex.out=list(ex.out)
for(i in 1:length(ex.out)){
junk=texPreview(obj = ex.out[[i]],stem = paste0('",fn,"Ex',i),fileDir = fd,imgFormat = 'png')
}
")

chnk3=
  paste0("lapply(list.files(fd,pattern = glob2rx('",fn,"*.png'),full.names=T),
             function(x) base64enc::dataURI(file = x, mime='image/png'))%>%
         loryR(images_per_page = 1, options = list(rewind=TRUE))
         ```
         ")
}
  



x=paste(header,chnk1,chnk2,chnk3,sep="
        \n
")

x

}

fn.src=function(fn,hlp.dir){
  fn.rd=paste0('../TFL/man/',fn,'.Rd')
  fn.hlp=file.path(hlp.dir,paste0(fn,'Help.html'))
  
  tools::Rd2HTML(Rd = fn.rd,out = fn.hlp)
  writeLines(paste0('<iframe width="100%" height="400" src="',fn.hlp,'" allowfullscreen></iframe>'))
}

createChapter=function(chapterName='Tables',chapterNum='01',funs=NULL){
  header<-paste0('# ',chapterName,'\n')
  if(!is.null(funs)) body<-sapply(funs$name,help.chunk,output = funs$type)
  cat(c(header,body),sep = '\n',file = paste0(chapterNum,'-',chapterName,'.Rmd'))
}

queryChapter=function(concept){
  x=lapply(concept,function(x) help.search(x,fields = 'concept',package = 'TFL',rebuild = F)$matches[,c('Name','Entry')])
  out=do.call('rbind',x)
  out=out[out$Entry%in%c('figure','table'),]
  names(out)[2]='output'
  return(out)
}
