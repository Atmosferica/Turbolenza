library(lattice)
library(ggplot2)
library(methods)
library(e1071)

source('functions.R')
source('turbulence_class.R')

filename <- list.files('.', pattern='*.dat') # listing all the files in the working dir
var_code <- sub('.dat','', filename) # removing the '.dat' at the end of the filename

for(i in 1:length(filename))
{
  # Extracted data from csv using the script convert_cvs.awk. 
  data <- read.csv(filename[i])
  # Converted data (of class data.frame) into an object of class turbulence
  turb <- as.turbulence(data)
  
  turb <- set_hvel(turb) # setting horizontal velocity
  turb <- set_direction(turb)  # setting direction
  
  
  source('Correlation.R')
  source('Periodigram.R')
}

