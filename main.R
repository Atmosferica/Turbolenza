library(lattice)
library(ggplot2)
library(methods)

source('functions.R')
source('turbulence_class.R')

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('./20151218.17r.dat')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

turb <- set_hvel(turb) # for setting horizontal_velocity in turbulence class
hor_vel <- get_hvel(turb)
#hor_velocity <- hor_vel[,1]
temp <- turb@t
hor_velocity <- get_zvel(turb)
hor_velocity <- hor_velocity+2

plot(temp,hor_velocity,type="p",pch=20)
cor(temp,hor_velocity)



acq.freq <- 10.
Npoint <- length(hor_velocity)
time <- Npoint*(1/acq.freq)
ts <- seq(0,time,1/acq.freq) 
f.0 <- 1/time
w <- 2*pi*f.0

X.k <- fft(hor_velocity)
ampiezze <- Mod(X.k[1:(length(X.k))/2])/Npoint
frequenze <- seq(0, acq.freq/2, length.out=length(ampiezze))
X.k[20:Npoint] <- 0+0i
hvel2 <- Mod(fft(X.k, inverse = TRUE)/(Npoint))

residuals <- hor_velocity - hvel2


XX.k <- fft(residuals)

ampiezze2 <- Mod(XX.k[1:(length(XX.k))/2])/Npoint

par(mfrow = c(2,2))
plot(hor_velocity ~ ts(1:length(hor_velocity)-1),t='l',xlab="Tempi[s]", ylab="Velocita`[m/s]")
lines(hvel2, col="red",lwd=1)
plot(ampiezze ~ frequenze, t="l", xlim=c(0,acq.freq/2),log="y", xlab="Frequenze[Hz]", ylab = "Potenza")
#plot(hvel2 ~ ts(1:length(hor_velocity)-1), t='l',xlab="Tempi[s]", ylab="Velocita` smooth [m/s]",ylim=c(0,4))
plot(residuals ~ ts(1:length(hor_velocity)-1), xlab="Tempi[s]", ylab="V1-V2[m/s]", t='l' )#,ylim=c(-2,2))
plot(ampiezze2 ~ frequenze, xlim=c(0,1),ylim=c(0.001,0.02), xlab="Frequenze[Hz]", ylab = "Potenza", lwd=0.2, type="l", pch=20, cex=0.1)
