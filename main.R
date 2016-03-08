library(lattice)
library(ggplot2)

source('functions.R')
source('turbulence_class.R')

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('./20160129.15r.dat')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

hor_velocity <- get_hvel(turb)



# Nota di servizio: son giunto alla conclusione che implementare una classe turbolenza
# fin dal principio sia un po' da mona, penso che quello che ho scritto vada cancellato tutto :D
# (in realtà poi ne parliamo a voce, tanto abbiamo due settimane!)

# This is only an attempt to make a first main for our project, it should be changed 
# during the work.
