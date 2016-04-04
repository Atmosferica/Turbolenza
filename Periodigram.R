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
hamming <- hamming/sum(hamming)*length(vel)
vel <- vel*hamming

# Importing cut times from file: I think it's better to import from file,
# because if you want to change the cut threshold you can touch the
# file instead of source code. 
# Maybe we can do the call in the main (for avoiding for() loop, little time
# expensive) or use a save()/load() paradigm...
cut_times <- read.table(paste(data_path, 'cut_freq', sep=''))
cut_freq <- 1/cut_times # Converting times into frequencies

#Creating a dir for the fft-graphs 
create_directory(paste(directory_dataset, '/grafici_fft/', sep=''))

# Here we're running the fft on the array of velocities

for(k in 1:length(cut_freq[,1])){
  
  #Creating a dir for the fft-graphs 
  png(paste(directory_dataset,"/grafici_fft/fft_cut_",round(cut_freq[k,1], 4),"Hz.png",sep = ''))
  par(mfrow=c(2, 2))
  data <- dofft(vel, 10)
  filt <- filter.data(data$freq, data$fft_vel, cut_freq[k,1])
  plot(data$peaks ~ data$freq, ylim=c(0.001,0.04), xlim=c(0.001,5), type='l',log=c('x','y'))
  plot(filt$peaks ~ filt$freq, ylim=c(0.001,0.04), xlim=c(0.001,5), type='l',log=c('x','y'))
  plot(vel/hamming ~ data$ts, type='l')
  vel_filt=Re(filt$vel)/hamming
  plot(vel_filt ~ data$ts, type='l', ylim=c(-1,1))
  dev.off()
  #temp <- recordPlot()
  #name <- paste(directory_dataset,"/grafici_fft/fft_cut_",round(cut_freq[k,1], 4),"Hz.png",sep = '')
  #print_plot(temp, 1200, 900, name)
  
}
