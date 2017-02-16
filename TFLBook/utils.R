help.chunk=function(pkg,fn,output){
if(!is.character(fn)) fn=as.character(substitute(fn))

header=paste0("## ",fn)

chnk1=
paste0(
"```{r results='asis',echo=FALSE}
fn.src('",fn,"','",pkg,"',hlp.dir)
```")

eval(parse(text=paste0('chnkExample<-readExample(',fn,')')))

if(output=='figure'){
  chnk2=paste0("```{r ",fn,", echo=FALSE,fig.show='hide',warning=FALSE}
")
  
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
  



x=paste(header,chnk1,chnk2,chnkExample,chnk3,sep="
        \n
")

x

}

fn.src=function(fn,pkg,hlp.dir){
  pkgpaths <- find.package(pkg)
  fn.rd <- utils:::index.search(fn, pkgpaths, TRUE)
  fn.hlp=file.path(hlp.dir,paste0(fn,'Help.html'))
  
  tools::Rd2HTML(Rd = utils:::.getHelpFile(fn.rd),out = fn.hlp)
  #fn.hlp=paste0(capture.output({tools::Rd2HTML(Rd = utils:::.getHelpFile(fn.rd))}),collapse='\n')
  writeLines(paste0('<iframe width="100%" height="400" src="',fn.hlp,'" allowfullscreen></iframe>'))
}

createChapter=function(chapterName='Tables',chapterNum='01',funs=NULL){
  header<-paste0('# ',chapterName,'\n')
  if(!is.null(funs)) body<-sapply(funs$name,help.chunk,pkg=funs$pkg,output = funs$type)
  cat(c(header,body),sep = '\n',file = paste0(chapterNum,'-',chapterName,'.Rmd'))
}

queryChapter=function(concept){
  x=lapply(concept,function(x) help.search(x,fields = 'concept',package = 'TFL',rebuild = F)$matches[,c('Package','Name','Entry')])
  out=do.call('rbind',x)
  out=out[out$Entry%in%c('figure','table'),]
  names(out)=c('pkg','Name','output')
  return(out)
}

readExample<-function (topic, package = NULL, lib.loc = NULL, character.only = FALSE, 
                       give.lines = FALSE, local = FALSE, echo = TRUE, verbose = getOption("verbose"), 
                       setRNG = FALSE, ask = getOption("example.ask"), prompt.prefix = abbreviate(topic, 
                                                                                                  6), run.dontrun = FALSE, run.donttest = interactive()) 
{
  if (!character.only) {
    topic <- substitute(topic)
    if (!is.character(topic)) 
      topic <- deparse(topic)[1L]
  }
  pkgpaths <- find.package(package, lib.loc, verbose = verbose)
  file <- utils:::index.search(topic, pkgpaths, TRUE)
  if (!length(file)) {
    warning(gettextf("no help found for %s", sQuote(topic)), 
            domain = NA)
    return(invisible())
  }
  packagePath <- dirname(dirname(file))
  pkgname <- basename(packagePath)
  lib <- dirname(packagePath)
  tf <- tempfile("Rex")
  tools::Rd2ex(utils:::.getHelpFile(file), tf, commentDontrun = !run.dontrun, 
               commentDonttest = !run.donttest)
  if (!file.exists(tf)) {
    if (give.lines) 
      return(character())
    warning(gettextf("%s has a help file but no examples", 
                     sQuote(topic)), domain = NA)
    return(invisible())
  }
  on.exit(unlink(tf))
  if (give.lines) 
    return(readLines(tf))
  if (pkgname != "base") 
    library(pkgname, lib.loc = lib, character.only = TRUE)
  if (!is.logical(setRNG) || setRNG) {
    if ((exists(".Random.seed", envir = .GlobalEnv))) {
      oldSeed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv), 
              add = TRUE)
    }
    else {
      oldRNG <- RNGkind()
      on.exit(RNGkind(oldRNG[1L], oldRNG[2L]), add = TRUE)
    }
    if (is.logical(setRNG)) {
      RNGkind("default", "default")
      set.seed(1)
    }
    else eval(setRNG)
  }
  x=readLines(tf)

  x=rmBlk('No test',x)
  x=rmBlk('Not run',x)
  paste(x,collapse='\n')
}

rmBlk=function(s,x){
  a=grep(s,x,fixed = T)
  if(length(a)==1) x=x[-a]
  if(length(a)>1){
    x=x[-unlist(sapply(data.frame(matrix(a,ncol=2,byrow = T)),function(y) seq(from=y[1],to=y[2])))] 
  }
  return(x)
}

ctl2html=function(ctl){
  header=c('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">',
           '<html xmlns="http://www.w3.org/1999/xhtml">',
           '<head>',
           '<title>NONMEM: CTL EXAMPLE</title>',
           '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />',
           '<link rel="stylesheet" type="text/css" href="R.css" />',
           '</head>',
           '<body>',
           '<pre>')
  body=readLines(ctl)
  footer=c('</pre>','</body>','</html>')
  cl=sapply(list(header,body,footer),function(x) paste(x,collapse='\n'))
  paste(unlist(cl),collapse = '\n')
}