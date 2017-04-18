#' @title Add minor ticks to identity scales in a ggplot
#' @description Add minor ticks to plot
#' @param p1 ggplot object
#' @param minorTickNum Number of minor ticks between major ticks
#' @examples 
#' p=ggplot(data.frame(x=rnorm(1000,0,1),y=rnorm(1000,0,1)),aes(x=x,y=y))+geom_point()
#' p%>%addMinorTicks(xScale='identity',yScale='identity',minorTickNum = 4)
#' @import ggplot2
#' @importFrom grid linesGrob gpar unit

addMinorTicks=function(p1,xScale=NULL,yScale=NULL,xForm=NULL,yForm=NULL,xLimit=NULL,yLimit=NULL,minorTickNum=1){
    minorTickNum=minorTickNum+2

    #xScale
    if(!is.null(xScale)){
    xScaleStr=as.character(xScale)[1]
    glineX = grid::linesGrob(y = grid::unit(c(0, .15),'cm'),  gp = grid::gpar(col = "black", size = 1)) 
    if(as.character(xScale)[1]=="identity"){
      p1=p1+ggplot2::scale_x_continuous(labels=eval(xForm), breaks=pretty_breaks(), limits=xLimit, trans=xScale,minor_breaks = minorBreaks(p1,'x',minorTickNum))
      p1=p1+lapply(ggplot2::ggplot_build(p1)$layout$panel_ranges[[1]]$x.minor_source,function(x) ggplot2::annotation_custom(glineX, xmin=x, xmax=x, ymin=-Inf, ymax=Inf))  
    }      
    if (xScaleStr%in%c("log10",'log')){
      p1=p1+ggplot2::annotation_logticks(base=ifelse(xScaleStr=='log10',10,exp(1)), sides="b", mid=unit(0.1, "cm"),ticks_per_base=2)
      
    }      
    }
    
    #yScale
    if(!is.null(yScale)){
    yScaleStr=as.character(yScale)[1]
    glineY = grid::linesGrob(x = unit(c(0, .15),'cm'),  gp = grid::gpar(col = "black", size = 1)) 
    if(as.character(yScale)[1]=="identity"){
      p1=p1+ggplot2::scale_y_continuous(labels=eval(yForm), breaks=pretty_breaks(), limits=yLimit, trans=yScale,minor_breaks = minorBreaks(p1,'y',minorTickNum))
      p1=p1+lapply(ggplot2::ggplot_build(p1)$layout$panel_ranges[[1]]$y.minor_source,function(y) ggplot2::annotation_custom(glineY, xmin=-Inf, xmax=Inf, ymin=y, ymax=y))
    }
    
    #Add in better ticks if the scale is log10
    if (yScaleStr%in%c("log10",'log')){
      p1=p1+ggplot2::annotation_logticks(base=ifelse(yScaleStr=='log10',10,exp(1)), sides="l", mid=grid::unit(0.1, "cm"))
    }}      
    
  return(p1)
}