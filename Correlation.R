library(lattice)
library(ggplot2)

source('functions.R')
source('turbulence_class.R')

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('./data/20160129.12r.dat')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

turb <- set_hvel(turb) # for setting horizontal_velocity in turbulence class
velH_T <- get_hvel(turb)
velZ_T <- get_zvel(turb)
Npoint <- length(velZ_T[,1])

par(mfrow=c(1,2))
c1 = cor(velH_T[1:Npoint-1,1],velH_T[1:Npoint-1,2])
plot(velH_T[1:Npoint-1,1],velH_T[1:Npoint-1,2],type="p",pch=20,xlab = "Velocity-H[m/s]", ylab = "Temperature[C]")

c2 = cor(velZ_T[1:Npoint-1,1],velZ_T[1:Npoint-1,2])
plot(velZ_T[1:Npoint-1,1],velZ_T[1:Npoint-1,2],type="p",pch=20,xlab = "Velocity-Z[m/s]", ylab = "Temperature[C]")
par(mfrow=c(1,1))
write(c1,stdout())
write(c2,stdout())

