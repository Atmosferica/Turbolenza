# library(lattice)
# library(ggplot2)
# library(gtable)
# library(methods)
# library(e1071)
#library(mwindow)
#library(foreach)
#library(doMC)


source('functions.R')
source('turbulence_class.R')


data_path <- "data/"   
#data_path <- "data/Licor/"
filename <- list.files(data_path, pattern='*.dat') # listing all the files in the working dir
var_code <- sub('.dat','', filename) # removing the '.dat' at the end of the filename
filename_tot=paste(data_path,filename, sep="")
#name_dir <- sub('data','grafici_output',sub('.dat','',filename_tot))
#create_directory('grafici_output')

#Used for Licor
name_dir <- sub('data/Licor','OutputLicor',sub('.dat','',filename_tot))
create_directory('OutputLicor')
#This cycle reads all the files and creates 3 matrix full of kurtosis and skewness coefficient

x_sk <-matrix(nrow=length(filename_tot), ncol=4)
y_sk <-matrix(nrow=length(filename_tot), ncol=4)
z_sk <-matrix(nrow=length(filename_tot), ncol=4)

for(i in 1:length(filename_tot))
{
  # Extracted data from csv using the script convert_cvs.awk. 
  # header=TRUE --> Essential! High performance decay for header=FALSE	
  data <- read.csv(filename_tot[i], header=TRUE)
  dati <-read.title.time(filename[i])
  righe<-length(filename[i])
  
  # Converted data (of class data.frame) into an object of class turbulence
  turb <- as.turbulence(data)
  
  turb <- set_hvel(turb) # setting horizontal velocity
  turb <- set_direction(turb)  # setting direction
  
  create_directory(name_dir[i])
  
  #source('Correlation.R')
  #source('Periodigram.R')
  #source('orbital_method.R')
  
  ##solo per Anna e Chiara!!##
  normal_path <- paste(name_dir[i], '/normal', sep='')
  create_directory(normal_path)
  
  ##Firt column: skewness; second column: kurtosis
   #Finding kurtosis-skewness for x-velocity
   x_vel <- get_uvel(turb)
   x <- x_vel[,1]   
   x_sk[i,]<-sk(x, dati) 

 
  # Finding kurtosis-skewness for y-velocity
   y_vel <- get_vvel(turb)
   y <- y_vel[,1]   
   y_sk[i,]<-sk(y, dati) 
   
  # Finding kurtosis-skewness for z-velocity
   z_vel <- get_zvel(turb)
   z <- z_vel[,1]   
   z_sk[i,]<-sk(z, dati) 
}

source('Gaussian.R')
