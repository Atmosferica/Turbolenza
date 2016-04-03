library(lattice)
library(ggplot2)
library(gtable)
library(methods)
library(e1071)
library(mwindow)
library(foreach)
library(doMC)


source('functions.R')
source('turbulence_class.R')


data_path <- "data/"
filename <- list.files(data_path, pattern='*.dat') # listing all the files in the working dir
var_code <- sub('.dat','', filename) # removing the '.dat' at the end of the filename
filename_tot=paste(data_path,filename, sep="")
name_dir <- sub('data','grafici_output',sub('.dat','',filename_tot))

for(i in 1:length(filename_tot))
{
  # Extracted data from csv using the script convert_cvs.awk. 
  data <- read.csv(filename_tot[i])
  # Converted data (of class data.frame) into an object of class turbulence
  turb <- as.turbulence(data)
  
  turb <- set_hvel(turb) # setting horizontal velocity
  turb <- set_direction(turb)  # setting direction
  
  create_directory(name_dir[i])
  
  source('Correlation.R')
  source('Periodigram.R')
}

