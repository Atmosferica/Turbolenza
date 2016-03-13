library(lattice)
library(ggplot2)

source('functions.R')
source('turbulence_class.R')

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('20160129.15r.dat')

create_directory('./grafici_output')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

turb <- set_hvel(turb) # for setting horizontal_velocity in turbulence class
vel_T <- get_hvel(turb)
vel <- vel_T[,1]
Npoint=length(vel)

filtered.data <- filter.data(vel,10,20)

par(mfrow = c(2,2))
plot(vel ~ filtered.data$tsv,t='l',xlab="Tempi[s]", ylab="Velocita`[m/s]")
lines(filtered.data$tsv,filtered.data$hvel2, type='l', col="red", lwd=4)
plot(filtered.data$peaks ~ filtered.data$freq, t='l', pch=20, xlab="Tempi[s]", ylab="Velocita`[m/s]",xlim=c(0,1),ylim=c(0,0.03))
plot(filtered.data$res ~ filtered.data$tsv, t='l', pch=20, xlab="Tempi[s]", ylab="Velocita`[m/s]")
plot(filtered.data$peaks2 ~ filtered.data$freq, t='l', pch=20, xlab="Tempi[s]", ylab="Velocita`[m/s]",xlim=c(0,1),ylim=c(0,0.03))
par(mfrow = c(1,1))

# par(mfrow = c(2,2))
# plot(vel[1:Npoint-1] ~ filtered.data[1:Npoint-1,1],t='l',xlab="Tempi[s]", ylab="Velocita`[m/s]")
# lines(filtered.data[1:Npoint-1,1],filtered.data[1:Npoint-1,6], type='l', col="red", lwd=4)
# plot(filtered.data[1:Npoint-1,4] ~ filtered.data[1:Npoint-1,3],t='l',pch=20,xlab="Tempi[s]", ylab="Velocita`[m/s]",xlim=c(0,1),ylim=c(0,0.03))
# plot(filtered.data[1:Npoint-1,2] ~ filtered.data[1:Npoint-1,1],t='l',pch=20,xlab="Tempi[s]", ylab="Velocita`[m/s]")
# plot(filtered.data[1:Npoint-1,5] ~ filtered.data[1:Npoint-1,3],t='l',pch=20,xlab="Tempi[s]", ylab="Velocita`[m/s]",xlim=c(0,1),ylim=c(0,0.03))
# par(mfrow = c(1,1))

