# library(lattice)
# library(ggplot2)
# library(gtable)
# library(methods)
library(e1071)
#library(mwindow)
#library(foreach)
#library(doMC)
library(moments)

#source('estrattore_blocchi.R')
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


#This cycle reads all the files and creates 3 matrix full of kurtosis and skewness coefficient

# sigma_totale_zvel <- NULL
# sigma_totale_yvel <- NULL
# sigma_totale_xvel <- NULL
count <-1

for(i in 1:length(filename_tot))
{

  # Extracted data from csv using the script convert_cvs.awk. 
  # header=TRUE --> Essential! High performance decay for header=FALSE	
  data <- read.csv(filename_tot[i], header=TRUE)
  
  if(length(data[,1]) >= min_camp*sonic_fqc*60 & count ==1){filename_dati <-filename[i]; count <-2}
  if(length(data[,1]) >= min_camp*sonic_fqc*60 & count ==2){filename_dati <-append(filename_dati, filename[i])}
  dati <- read.title.time(filename[i])
 
  # Converted data (of class data.frame) into an object of class turbulence
  turb <- as.turbulence(data)
  
  turb <- set_hvel(turb) # setting horizontal velocity
  turb <- set_direction(turb)  # setting direction
  
  #create_directory(name_dir[i])
  cat(name_dir[i],"\n")
  #cat("* Perfoming correlation graph...","\n")
  #source('Correlation.R')
  
  #cat("* Performing FFT analysis...","\n")
  #source('Periodigram.R')
  
  #cat("* Performing Markovian test...","\n")
  #source('markov_test.R')
  cat("* File: ",filename_tot[i],"..done!\n")
  
  #source('orbital_method.R')
  
}

filename_dati_tot=paste(data_path,filename_dati, sep="")

# png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("std_blocks_totale_zvel_",sub(".dat",'',filename[i]),"wvel.png",sep = '')))
# plot(sigma_totale_zvel)
# dev.off()
#    
# png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("std_blocks_totale_yvel_",sub(".dat",'',filename[i]),"wvel.png",sep = '')))
# plot(sigma_totale_yvel)
# dev.off()
# 
# png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("std_blocks_totale_xvel_",sub(".dat",'',filename[i]),"wvel.png",sep = '')))
# plot(sigma_totale_xvel)
# dev.off()

cat("* Perfoming Gaussian con filtro...","\n")
source('Gaussian.R')

cat("* Perfoming Gaussian senza filtro...","\n")
#source('senza_filtro.R')

cat("* Perfoming Wind...","\n")
#source('wind.R')

cat("* Perfoming Kolmogorov-Smirnoff test...","\n")
#source('kolmogorov.R')
#source('kolm_senza_filtro.R') 

