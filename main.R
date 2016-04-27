library(lattice)
library(ggplot2)
library(gtable)
library(methods)
library(e1071)
#library(mwindow)
#library(foreach)
#library(doMC)

source('estrattore_blocchi.R')
source('functions.R')
source('turbulence_class.R')


data_path <- path_dir
filename <- list.files(data_path, pattern='*.dat') # listing all the files in the working dir
var_code <- sub('.dat','', filename) # removing the '.dat' at the end of the filename
filename_tot=paste(data_path,filename, sep="")
name_dir <- sub('data','grafici_output',sub('.dat','',filename_tot))

create_directory('grafici_output')
create_directory('grafici_output/Fontanella1')
create_directory('grafici_output/Fontanella2')
create_directory('grafici_output/Fontanella1/Emission')
create_directory('grafici_output/Fontanella1/Control')
create_directory('grafici_output/Fontanella1/LiCor')
create_directory('grafici_output/Fontanella2/Emission')
create_directory('grafici_output/Fontanella2/Control')
create_directory('grafici_output/Fontanella2/LiCor')

for(i in 1:length(filename_tot))
{
  # Extracted data from csv using the script convert_cvs.awk. 
  # header=TRUE --> Essential! High performance decay for header=FALSE	
  data <- read.csv(filename_tot[i], header=TRUE)
  # Converted data (of class data.frame) into an object of class turbulence
  turb <- as.turbulence(data)
  
  turb <- set_hvel(turb) # setting horizontal velocity
  turb <- set_direction(turb)  # setting direction
  
  create_directory(name_dir[i])
  cat(name_dir[i],"\n")
  cat("* Perfoming correlation graph...","\n")
  #source('Correlation.R')
  
  cat("* Performing FFT analysis...","\n")
  #source('Periodigram.R')
  
  cat("* Performing Markovian test...","\n")
  source('markov_test.R')
  cat("* File: ",filename_tot[i],"..done!\n")
}

