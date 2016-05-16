n<-0
#Here the cycle starts: it reads all the files.Inside this cycle, there is
#another for cycle that works C
for(fl in 1:length(filename_tot))
{
  if (fl==1) {
    # Extracted data from csv using the script convert_cvs.awk. 
    # header=TRUE --> Essential! High performance decay for header=FALSE	
    data <- read.csv(filename_tot[fl], header=TRUE)
    dati <- read.title.time(filename[fl])
    mem<-dati[1]
  }
  
  if (fl!=1){
    data <- read.csv(filename_tot[fl], header=TRUE)
    dati <- read.title.time(filename[fl])

    if (dati[1]!=mem)  {

      for(counter in (n+1):(fl-1))  {
        
        data <- read.csv(filename_tot[counter], header=TRUE)
        info <- read.title.time(filename[counter])
        
        # Converted data (of class data.frame) into an object of class turbulence
        turb <- as.turbulence(data)
        turb <- set_hvel(turb) # setting horizontal velocity
        turb <- set_direction(turb)  # setting direction
        
        cat(name_dir[counter],"\n")
        path_output <- paste('grafici_output/', line, '/',info[1], '/', sep = '')  
        create_directory(path_output);
        
        path_output_new<-paste(path_output, "block/", sep ='')
        create_directory(path_output_new)
        
        f_cut_up <- 0.015
        f_cut_down <- 0.005
        
        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        
        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        down_smooth<-NULL
        tot_smooth<-NULL
        hamming <- hamming.window(length(h))
        hamming <- hamming/sum(hamming)*length(h)
        h <- h*hamming
        fft_h<- dofft(h,sonic_fqc)
        down_smooth<-LowPassfilter.data(fft_h$freq,fft_h$fft_vel,f_cut_up)
        tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
        h <- Re(tot_smooth$vel)/hamming 
        
        wind_plot(h, path_output, info, 1)
        

        theta <- get_direction(turb)
        theta_plot(theta, path_output, info, 1)
        if (counter==n+1){ theta_tot <-theta}
        else {theta_tot <-append(theta_tot,theta)}
        
    }
     
      n <- counter
      theta_plot(theta_tot, path_output, info, 0)
      
    }
    if (fl==length(filename_tot)) {

      for(counter in (n+1):fl)  {

        data <- read.csv(filename_tot[counter], header=TRUE)
        info <- read.title.time(filename[counter])
        
        turb <- as.turbulence(data)
        turb <- set_hvel(turb) # setting horizontal velocity
        turb <- set_direction(turb)  # setting direction
        
        cat(name_dir[counter],"\n")
        path_output <- paste('grafici_output/', line, '/',info[1], '/', sep = '')
        create_directory(path_output)
        
        path_output_new<-paste(path_output, "block/", sep ='')
        create_directory(path_output_new)
        
        f_cut_up <- 0.015
        f_cut_down <- 0.005
        
        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        
        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        down_smooth<-NULL
        tot_smooth<-NULL
        hamming <- hamming.window(length(h))
        hamming <- hamming/sum(hamming)*length(h)
        h <- h*hamming
        fft_h<- dofft(h,sonic_fqc)
        down_smooth<-LowPassfilter.data(fft_h$freq,fft_h$fft_vel,f_cut_up)
        tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
        h <- Re(tot_smooth$vel)/hamming 
        
        wind_plot(h, path_output, info, 1)
        
        ##Finding kurtosis-skewness and mean-sd for velocity direction.
        ##firth column: date; second : hour
        ##third column: mean; fourth column: sd   
        theta <- get_direction(turb)
        theta_plot(theta, path_output, info, 1)
        if (counter==n+1){ theta_tot <-theta}
        else {theta_tot <-append(theta_tot,theta)}
        
        
  }
      
       n <- counter
       theta_plot(theta_tot, path_output, info, 0)
      
    }
    mem<-dati[1]
  }
  
}


