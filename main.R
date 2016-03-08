library(lattice)
library(ggplot2)

source('functions.R')
source('turbulence_class.R')

#plot.fourier <- function(fourier.series, f.0, ts){
#  w <- 2*pi*f.0
#  segnale.totale <-sapply(ts, function(t) fourier.series(t,w))
#  plot(ts, segnale.totale, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
#}

#plot.frequency.specturum <- function(X.k, xlimits=c(0,length(X.k))) {
#  plot.data <- cbind(0:(length(X.k)-1), Mod(X.k))
#  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2]
#  
#  plot(plot.data, t="h", lwd=2, main="", xlab = "Frequenza", ylab = "Potenza", xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
#}

# Extracted data from csv using the script convert_cvs.awk. 
data <- read.csv('./20160129.15r.dat')

# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

turb <- set_hvel(turb) # for setting horizontal_velocity in turbulence class
hor_velocity <- get_hvel(turb) 



#commento --- e` cambiato il type quindi questa cosa non vale piu`


#acq.freq <- 10.
#Npoint <- length(hor_velocity)
#time <- Npoint*(1/acq.freq)
#ts <- seq(0,time,1/acq.freq)
#f.0 <- 1/time
#w <- 2*pi*f.0

#X.k <- fft(hor_velocity)
#ampiezze <- Mod(X.k[1:(length(X.k))/2])/Npoint
#frequenze <- seq(0, acq.freq/2, length.out=length(ampiezze))
#X.k[20:Npoint] <- 0+0i
#hvel2 <- Mod(fft(X.k, inverse = TRUE)/(Npoint))

#residuals=hor_velocity - hvel2


#par(mfrow = c(2,2))
#plot(hor_velocity ~ ts(1:length(hor_velocity)),t='l',xlab="Tempi[s]", ylab="Velocita`[m/s]")
#plot(hvel2 ~ ts(1:length(hor_velocity)-1), t='l',xlab="Tempi[s]", ylab="Velocita` smooth [m/s]")
#plot(residuals ~ ts(1:length(hor_velocity)-1), t='l' )
#plot(ampiezze ~ frequenze, t="l", xlim=c(0,acq.freq/2),log="y", xlab="Frequenze[Hz]", ylab = "Potenza")

