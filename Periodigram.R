
# Set velocity --------------------------------------------------------------

vel_T <- get_zvel(turb)
vel <- vel_T[,1]
Npoint=length(vel)

# Creating fft-graph directory ----------------------------------------------

create_directory(paste(name_dir[i], '/grafici_fft/', sep=''))

# Data windowing ------------------------------------------------------------

# Hann's window give instability to the boundaries
# hanning <- hanning.window(length(vel))
# hanning <- hanning/sum(hanning)*length(vel)
# vel <- vel*hanning

# Trying Hamming's window to avoid instability (should be !=0 at
# boundaries)

hamming <- hamming.window(length(vel))
hamming <- hamming/sum(hamming)*length(vel)
vel <- vel*hamming
cat("Velocity dataset size: ",Npoint,"\n")

# Import cut_freq from file --------------------------------------------------

cut_times <- read.table('data/cut_freq')
cut_freq <- 1/cut_times # Converting times into frequencies

# Perform the FFT ------------------------------------------------------------
fft_tm <- system.time(
	data <- dofft(vel^2, sonic_fqc) #ATTENTION! Some data set are sampled at 20Hz
)
# Plotting fft with ggplot() to solve the bug found by Stefano with 
# the option header=F in read.csv() (graph superimposed)
# Note: you don't need print_plot() anymore, you can use ggsave()
g1 <- ggplot(data=data, aes(x=freq, y=peaks)) + geom_line(, colour='black') + 
      scale_y_log10() + scale_x_log10() + ggtitle('FFT Z Velocity') +
      xlab('Frequencies (Hz)') + ylab('FFT Power Spectrum [dB]')
g1
ggsave(paste(name_dir[i], '/grafici_fft/fft_nofilter_z.png', sep=''))
rm(g1)
cat("FFT performed in: ",fft_tm,"\n")

# FFT filter -----------------------------------------------------------------

for(k in 1:length(cut_freq[,1])){
  
  # filt_time is needed for doing benchmark and measuring time
  # of execution of a function (in this case FFT)
  filt_time <- system.time(
	  filt <- LowPassfilter.data(data$freq, data$fft_vel, cut_freq[k,1])
  )
  cat("FILT performed in: ",filt_time,"\n")
  
  # Printing to plot and saving
  g1 <- ggplot(data=filt, aes(x=freq, y=peaks)) + geom_line(colour='black') + 
    scale_y_log10() + scale_x_log10() + ggtitle(paste('FFT: high-pass filter at ', round(cut_freq[k,1], 4),"Hz.png", sep='')) +
    xlab('Frequencies (Hz)') + ylab('')
  g1
  ggsave(paste(name_dir[i], '/grafici_fft/fft_cut_',round(cut_freq[k,1], 4),"Hz.png", sep=''))
  rm(g1)

 }
