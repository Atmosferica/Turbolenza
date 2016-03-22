###### Disclaimer: if you want to run this script alone, comment
# the unused scripts in main.R instead of importing here the dataset

vel_T <- get_zvel(turb)
vel <- vel_T[,1]
Npoint=length(vel)


#***** Hann's window give instability to the boundaries
# hanning <- hanning.window(length(vel))
# hanning <- hanning/sum(hanning)*length(vel)
# vel <- vel*hanning
# #plot(hanning)

#***** Trying Hamming's window to avoid instability (should be !=0 at
#***** boundaries)

hamming <- hamming.window(length(vel))
hamming <- hamming/sum(hanning)*length(vel)
vel <- vel*hamming

#filtered_data <- stft(vel, wtype='hanning.window')
#plot(filtered_data, ylim=c(0,10))

par(mfrow=c(2,2))
data <- dofft(vel,10)
filt <- filter.data(data$freq,data$fft_vel,0.01)
plot(data$peaks ~ data$freq, ylim=c(0.001,0.02), xlim=c(0.001,5), type='l',log=c('x','y'))
plot(filt$peaks ~ filt$freq, ylim=c(0.001,0.02), xlim=c(0.001,5), type='l',log=c('x','y'))
plot(vel/hamming ~ data$ts, type='l')
vel_filt=Re(filt$vel)/hamming
plot(vel_filt ~ data$ts, type='l', ylim=c(-1,1))
