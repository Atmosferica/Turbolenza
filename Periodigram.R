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
cat("Velocity dataset size: ",Npoint,"\n")



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

fft_tm <- system.time(
	data <- dofft(vel, 20) #I Bresciani lo fanno diversamente
)
# Plotting fft with ggplot() to solve the bug found by Stefano with 
# the option header=F in read.csv() (graph superimposed)
# Note: you don't need print_plot() anymore, you can use ggsave()
g1 <- ggplot(data=data, aes(x=freq, y=peaks)) + geom_line(, colour='black') + 
      scale_y_log10() + scale_x_log10() + ggtitle('FFT') +
      xlab('Frequencies (Hz)') + ylab('Amplitude')
g1
ggsave(paste(directory_dataset, '/grafici_fft/fft_nofilter.png', sep=''))
rm(g1)

cat("FFT performed in: ",fft_tm,"\n")
for(k in 1:length(cut_freq[,1])){
  
  # filt_time is needed for doing benchmark and measuring time
  # of execution of a function (in this case FFT)
  filt_time <- system.time(
	  filt <- filter.data(data$freq, data$fft_vel, cut_freq[k,1])
  )
  cat("FILT performed in: ",filt_time,"\n")
  
  # Printing to plot and saving
  g1 <- ggplot(data=filt, aes(x=freq, y=peaks)) + geom_line(colour='black') + 
    scale_y_log10() + scale_x_log10() + ggtitle(paste('FFT: high-pass filter at ', round(cut_freq[k,1], 4),"Hz.png", sep='')) +
    xlab('Frequencies (Hz)') + ylab('Amplitude')
  g1
  ggsave(paste(directory_dataset, '/grafici_fft/fft_cut_',round(cut_freq[k,1], 4),"Hz.png", sep=''))
  rm(g1)

 }
