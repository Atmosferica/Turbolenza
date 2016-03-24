
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


# Must adapt path for usage with both Linux and Windows
# (linux uses ../../dir, windows C:\\....)
# I think it can be solved with a different assignation of
# path using the result of Sys.info()[['sysname]]
# and the usage of normalizePath()... should try as soon as possible.