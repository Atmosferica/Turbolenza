## plot wind direction and modulus versus time, for one hour and for one day.

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
        
        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        theta <- get_direction(turb)
        
        if (counter==n+1){ theta_tot <-theta; wind_tot<-h}
        else {theta_tot <-append(theta_tot,theta); wind_tot <-append(wind_tot,h) }
        
        wind_plot(h, theta, path_output, info, 1)
        
    }
     
      n <- counter
      wind_plot(wind_tot, theta_tot, path_output, info, 0)
      
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

        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        theta <- get_direction(turb)
        
        if (counter==n+1){ theta_tot <-theta; wind_tot<-h}
        else {theta_tot <-append(theta_tot,theta); wind_tot <-append(wind_tot,h) }
        
        wind_plot(h, theta, path_output, info, 1)
        
      }
      
      n <- counter
      wind_plot(wind_tot, theta_tot, path_output, info, 0)
    }
    mem<-dati[1]
  }
  
}


