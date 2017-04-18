#' @title Give minor breaks
#' @description Internal TFL function
#' @export
#' @import ggplot2
#' 
minorBreaks=function(p1,ax,n,Limit=NULL,Scale='identity'){
  g=suppressWarnings(ggplot2::ggplot_build(p1))
  gr=g$layout$panel_ranges[[1]]
  gr.val=gr[grep('major.source|label|range',names(gr),value = T)]
  a=gr.val[[paste0(ax,'.major_source')]]
  a1=c()
  breaks.out=ggplot2::waiver()
  if(length(a)>1){
  for(i in 1:(length(a)-1)) {
    a1=c(a1,seq(a[i],a[i+1],length.out=n))
  }
    a1=unique(a1)
    
    breaks.out=a1[which(!a1%in%a)]
    }

  return(breaks.out)
  
}