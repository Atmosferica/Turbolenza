library(lattice)
library(ggplot2)

source('functions.R')
source('turbulence_class.R')

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('./20160129.15r.dat')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

# This is only an attempt to make a first main for our project, it should be changed 
# during the work.
