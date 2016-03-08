



xs <- seq(-2*pi,2*pi,pi/100) #dataset temporale
armonica.1 <- sin(3*xs)      #punti arm1
armonica.2 <- sin(10*xs)     #punti arm2
#par(mfrow = c(1,2))          #dico che voglio due grafici
#plot(xs,armonica.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
#plot(xs,armonica.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

segnale.tot <- 0.5 * armonica.1 + 0.25 * armonica.2
plot(xs,segnale.tot,type="l"); title("Somma"); abline(h=0,lty=3)

plot.fourier <- function(fourier.series, f.0, ts){
  w <- 2*pi*f.0
  segnale.totale <-sapply(ts, function(t) fourier.series(t,w))
  plot(ts, segnale.totale, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}

plot.fourier(function(t,w) {sin(w*t)}, 2, ts=seq(0,1,1/100))

##bla bla bla
acq.freq <- 100
time <- 6
ts <- seq(0,1,1/acq.freq)
f.0 <- 1/time
w <- 2*pi*f.0

componente.continua <- 0
frequenze <- c(3,10)
ritardo <- c(0,0)
ampiezza <- c(.5,.25)

f <- function(t,w){
  componente.continua + sum(ampiezza*sin(frequenze*w*t+ritardo))
}

#plot.fourier(f,f.0,ts)

plot.frequency.specturum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data <- cbind(0:(length(X.k)-1), Mod(X.k))
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2]
  
  plot(plot.data, t="h", lwd=2, main="", xlab = "Frequenza", ylab = "Potenza", xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

segnale.totalissimo <- sapply(ts, function(t) f(t,w))
X.k <- fft(segnale.totalissimo)
plot.frequency.specturum(X.k, xlimits=c(0,20))









