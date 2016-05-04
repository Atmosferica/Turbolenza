
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
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
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
  plot(y=mov_wind, x=frequencies, ylim=c(-2, 0.2), type='h', col=ifelse(sign(mov_wind)==1, color_palette1, color_palette2),
       yaxt='n', xaxt='n')
  axis(labels=T, side=2, at=c(seq(from=-2, to=0.2, length.out=10)), las=2, yaxs='i', xaxs='i')
  axis(labels=T, side=1, at=c(seq(from=frequencies[1], to=max(frequencies), 
                                  length.out=50)), las=2, yaxs='i', xaxs='i')
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

#this function saves date and time as different elments of a data frame, getting them from 
#the name of the file itself
read.title.time <- function(filename_tot) {
  prova <- strsplit(filename_tot, "[.]")
  date <- prova[[1]][1]
  time <- sub('r', '', prova[[1]][2])
  name_file <- sub('.dat','',filename_tot)
  dati <- c(as.numeric(date), as.numeric(time))
  return(dati)
}

#this function calculates skewness and kurtosis
sk<- function(x, y, l) {
  skew<-skewness(x, na.rm = TRUE)
  kurt<-(kurtosis(x, na.rm = TRUE) - 3)
  m_sk<- c(y[1],y[2],skew,kurt)
    return(m_sk)
}


#returns skewness and kurtosis for one block
sk.blocks<-function(time_stamp, x, block, dim_bl,dati){
  
  sig <- signal.partition(time_stamp, x, block, dim_bl)
  vector_blocks  <- with(sig,value)
  m_sk<-sk(vector_blocks, dati) 
  m_sk <-t(m_sk)
  
  return(m_sk)
}

#plot skewness and kurtosis
sk_plot <-function(m_sk, path_output, coord ){
  png(paste(path_output, "Skeweness+Kurtosis_", coord, ".png",sep=""));
  par(mfrow=c(2,1));
  plot(m_sk[,2], m_sk[,3], ylim = c(min(m_sk[,3])-1, max(m_sk[,3])+1), xlab = 'Time (hours)', ylab = 'Skewness', type = 'p', main = paste('Skewness_',coord, sep=''),bg = "blue",col="blue", pch = 20, cex = 2 )
  abline(h=0)
  plot(m_sk[,2], m_sk[,4], ylim = c(min(m_sk[,4])-1, max(m_sk[,4])+1), xlab = 'Time (hours)', ylab = 'Kurtosis', type = 'p', main = paste('Kurtosis_',coord, sep=''),bg = "blue",col="blue", pch = 20, cex = 2 )
  abline(h=0)
  dev.off()
}

#plot skewness and kurtosis in x, y and z in the same graphic
sk_plot.xyzh <-function(x_sk, y_sk, z_sk, h_k,path_output ){

  png(paste(path_output, "Skeweness+Kurtosis_xyzh", ".png",sep=""));
  par(mfrow=c(2,1));
  
  plot(x_sk[,2], x_sk[,3],  ylim = c(-6, 6), xlab = 'Time (hours)', ylab = 'Skewness', type = 'p', main = paste('Skewness', sep=''),col="blue", pch = 1, cex = 2, sub = 'x: blue     y: red     z: green    h:black' )
    points(y_sk[,2], y_sk[,3], col="red", pch = 1, cex = 2 )
    points(z_sk[,2], z_sk[,3], col="green", pch = 1, cex = 2 )
    points(h_sk[,2], h_sk[,3], col="black", pch = 1, cex = 2 )
    abline(h=0)
  
  
  plot(x_sk[,2], x_sk[,4], ylim = c(-6, 6), xlab = 'Time (hours)', ylab = 'Kurtosis', type = 'p', main = paste('Kurtosis', sep=''), col="blue", pch =1, cex = 2, sub = 'x: blue     y: red     z: green     h:black' )
    points(y_sk[,2], y_sk[,4], col="red", pch = 1, cex = 2 )
    points(z_sk[,2], z_sk[,4], col="green", pch = 1, cex = 2 )
    points(h_sk[,2], h_sk[,4], col= "black", pch = 1, cex = 2 )
    abline(h=0)

  dev.off()
}

# graphical display of data using and histogram and plot of the normal distribution with  data's mean and sd
print.hist.gauss<-function(x, path_output, coord, tempo) {
  hist(x, main= paste (tempo, "_Histogram_", coord , sep = ''), xlab = paste ("vel_", coord , sep = ''), probability = TRUE, col = "cornflowerblue")
  x0<-seq(min(x), max(x), length.out= 100)
  lines(x0, dnorm(x0, mean = mean(x), sd= sd(x)), lwd=3, type = "l" )
  scatplot <- recordPlot()
  name <- paste(path_output, "/", tempo, "_Histogram_",coord,".png",sep = '')
  print_plot(scatplot, 1200, 900, name)
  rm(scatplot)
}


signal.block.mean <- function(time.stamp, signal, block.length=300) {   # lunghezza del blocco 
  time.index <- time.stamp %/% block.length                             # time.index numero di blocchi 
  part <- aggregate(signal, by=list(time.index), FUN=mean, na.rm=TRUE)  # fa la media del vettore passate suddividendolo in blocchi 
  names(part) <- c("time.stamp", "value")                               # inserisce l'header
  part$time.stamp <- part$time.stamp * block.length
  return(part)
}

signal.partition <- function(time.stamp, signal, block, block.length=300) {
  time.index <- time.stamp %/% block.length
  separate.indices <- unique(time.index)                #rimuove gli elementi doppi del vettore/data frame che gli viene passato
  n.blocks <- length(separate.indices)                  #quanti blocchi ho in totale
  if(block <= 0 | block > n.blocks) return(NULL)        
  current.block.idx <- which(time.index == separate.indices[block])       #scegli il blocco che vuoi
  current.block <- signal[current.block.idx]                              #isolalo
  current.time.stamp <- time.stamp[current.block.idx]
  result <- data.frame(time.stamp = current.time.stamp, value = current.block)  #rende il blocco da solo
  return(result)
}
