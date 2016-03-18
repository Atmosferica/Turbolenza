library(lattice)
library(ggplot2)
library(methods)
library(e1071)

source('functions.R')
source('turbulence_class.R')

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('data/20160129.14r.dat')

#create_directory('./grafici_output')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

turb <- set_hvel(turb) # for setting horizontal_velocity in turbulence class
vel_T <- get_zvel(turb)
vel <- vel_T[,1]
Npoint=length(vel)



hanning <- hanning.window(length(vel))
hanning <- hanning/sum(hanning)*length(vel)
vel <- vel*hanning
#plot(hanning)

#filtered_data <- stft(vel, wtype='hanning.window')
#plot(filtered_data, ylim=c(0,10))

par(mfrow=c(2,1))
data <- dofft(vel,10)
plot(data$peaks ~ data$freq, ylim=c(0.001,0.02), xlim=c(0.001,5), type='l',log=c('x','y'))
filt <- filter.data(data$freq,data$fft_vel,1)
plot(filt$peaks ~ filt$freq, ylim=c(0.001,0.02), xlim=c(0.001,5), type='l',log=c('x','y'))




#filtered.data <- filter.data(vel,10,20)

#par(mfrow = c(1,2))
#plot(vel ~ filtered.data$tsv,t='l',xlab="Tempi[s]", ylab="Velocita`[m/s]")
#lines(filtered.data$tsv,filtered.data$hvel2, type='l', col="red", lwd=4)
#plot(filtered.data$peaks ~ filtered.data$freq, t='l', pch=20, xlab="Freq[Hz]", ylab="I",xlim=c(0,1),ylim=c(0,0.03))
#plot(filtered.data$res ~ filtered.data$tsv, t='l', pch=20, xlab="Tempi[s]", ylab="Velocita`[m/s]")
#plot(filtered.data$peaks2 ~ filtered.data$freq, t='l', pch=20, xlab="Freq[Hz]", ylab="I",xlim=c(0.001,1),ylim=c(0.001,0.3))#, log=c('x','y'))
#par(mfrow = c(1,1))

# par(mfrow = c(2,2))
# plot(vel[1:Npoint-1] ~ filtered.data[1:Npoint-1,1],t='l',xlab="Tempi[s]", ylab="Velocita`[m/s]")
# lines(filtered.data[1:Npoint-1,1],filtered.data[1:Npoint-1,6], type='l', col="red", lwd=4)
#plot(filtered.data[1:Npoint-1,4] ~ filtered.data[1:Npoint-1,3],t='l',pch=20,xlab="Tempi[s]", ylab="Velocita`[m/s]",xlim=c(0,1),ylim=c(0,0.03))
# plot(filtered.data[1:Npoint-1,2] ~ filtered.data[1:Npoint-1,1],t='l',pch=20,xlab="Tempi[s]", ylab="Velocita`[m/s]")
# plot(filtered.data[1:Npoint-1,5] ~ filtered.data[1:Npoint-1,3],t='l',pch=20,xlab="Tempi[s]", ylab="Velocita`[m/s]",xlim=c(0,1),ylim=c(0,0.03))
# par(mfrow = c(1,1))

