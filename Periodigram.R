###### Disclaimer: if you want to run this script alone, comment
# the unused scripts in main.R instead of importing here the dataset

vel_T <- get_zvel(turb)
vel <- vel_T[,1]
Npoint=length(vel)



hanning <- hanning.window(length(vel))
hanning <- hanning/sum(hanning)*length(vel)
vel <- vel*hanning
#plot(hanning)

#filtered_data <- stft(vel, wtype='hanning.window')
#plot(filtered_data, ylim=c(0,10))

par(mfrow=c(2,2))
data <- dofft(vel,10)
filt <- filter.data(data$freq,data$fft_vel,0.01)
plot(data$peaks ~ data$freq, ylim=c(0.001,0.02), xlim=c(0.001,5), type='l',log=c('x','y'))
plot(filt$peaks ~ filt$freq, ylim=c(0.001,0.02), xlim=c(0.001,5), type='l',log=c('x','y'))
plot(vel/hanning ~ data$ts, type='l')
vel_filt=Re(filt$vel)/hanning
plot(vel_filt ~ data$ts, type='l', ylim=c(-1,1))
