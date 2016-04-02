
#This is a simple function for plotting on a file. Once you have the object "plot",
#you pass it as an argument together with width, heigth and the name of the file where do you want to plot.
# Check if it works with ggplot2 too!!
print_plot<-function(x,w,h,filename){
  png(file=filename, width=w, height=h, units='px', res=96, type='cairo')
  print(x)
  dev.off()
  rm(x)
}


#This function is only a concatenation of dir.exist() and dir.create(): when you're sure that there is a dir
#with the name chosen, it changes and returns the path to let you use it as a parameter in the filename
create_directory<-function(path){
  x<- getRversion()
  if(x < "3.2.0"){
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }else{
    if(dir.exists(path)!=TRUE){
      dir.create(path)
    }
    path<-paste(path,'/',sep='')
  }
  #return(path)
}


grafico_mwind<-function(mov_wind, frequencies, string=NULL, ora=NULL, inf=NULL, sup=NULL){
  par(ps=12,cex=1, cex.main=1)
  #create_directory(paste(destination_path,'/',ora,'/[',inf,",",sup,')',sep=''))
  mov_wind <- mov_wind*100 #for having a percentual slope 
  rbPal1 <- colorRampPalette(c('yellow','red'))
  rbPal2 <- colorRampPalette(c('blue', 'green'))
  color_palette1 <- rbPal1(10)[as.numeric(cut(mov_wind, breaks = 10))]
  color_palette2 <- rbPal2(10)[as.numeric(cut(mov_wind, breaks = 10))]
  par(cex=1.2, lwd=4.5, cex.axis=0.8)
  #layout(matrix(1:2,ncol=2),width=c(2,1), height=c(1,1))
  plot(y=mov_wind, x=frequencies, ylim=c(-1.2, 0.2), type='h', col=ifelse(sign(mov_wind)==1, color_palette1, color_palette2),
       yaxt='n', xaxt='n')
  axis(labels=T, side=2, at=c(seq(from=-3.5, to=3.5, by=0.5)), las=2, yaxs='i', xaxs='i')
  axis(labels=T, side=1, at=c(seq(from=1951, to=2014, by=5)), las=2, yaxs='i', xaxs='i')
  par(cex=1.2, lwd=1, cex.axis=1)
  #grid_for_inter()
  plot <- recordPlot()
  #print_plot(plot,900,600,paste(destination_path, '/mwind_',string,'_[' ,inf," , ",sup,' )_',ora,'.png',sep='') )
  return()
}




#********************************************************************
# This function is for having a double y-axis with ggplot2
# (thanks to https://rpubs.com/kohske/dual_axis_in_ggplot2).
# Hadley Wickham didn't implement this functionality in ggplot2 because he thinks
# (and some publications too) that's profoundly wrong to use double 
# y-axis in plots... it should be used only if you want to represent
# quantities with two different unit of measure.
#********************************************************************

dual_axis <- function(p1, p2){
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))

  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", select = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)

  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

  return(g)
}