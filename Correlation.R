library(lattice)
library(ggplot2)
library(methods)


source('functions.R')
source('turbulence_class.R')

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('./20160129.15r.dat')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

turb <- set_hvel(turb) # for setting horizontal_velocity in turbulence class
velH_T <- get_hvel(turb)
velZ_T <- get_zvel(turb)

par(mfrow=c(1,2))
c1 = cor(velH_T[,1],velH_T[,2])
scatplot <- plot(velH_T[,1], velH_T[,2], type="p", pch=20, xlab = "Velocity-H[m/s]", ylab = "Temperature[C]", 
	main='Scatterplot horizontal velocity vs. temperature', sub=paste('Correlation: ', c1, sep=''))


c2 = cor(velZ_T[,1],velZ_T[,2])
scatplot <- plot(velZ_T[,1], velZ_T[,2], type="p", pch=20, xlab = "Velocity-Z[m/s]", ylab = "Temperature[C]",
	main='Scatterplot vertical velocity vs.  temperature', sub=paste('Correlation: ', c2, sep=''))

scatplot <- recordPlot()
print_plot(scatplot, 600, 400, 'scatterplot.png')
rm(scatplot)


#write(c1,stdout())
#write(c2,stdout())
