
for(fl in 1:length(filename_tot))
{
  #fl<-1
  # Extracted data from csv using the script convert_cvs.awk. 
  # header=TRUE --> Essential! High performance decay for header=FALSE	
  data <- read.csv(filename_tot[fl], header=TRUE)
  dati <- read.title.time(filename[fl])
  
  # Converted data (of class data.frame) into an object of class turbulence
  turb <- as.turbulence(data)
  
  # turb <- set_hvel(turb) # setting horizontal velocity
  turb <- set_direction(turb)  # setting direction
  
  #create_directory(name_dir[i])
  cat(name_dir[fl],"\n")
  
  path_output <- paste('grafici_output/', line, '/',dati[1], '/', sep = '')  
  create_directory(path_output)

  ##Firt column: skewness; second column: kurtosis
  #Finding kurtosis-skewness for x-velocity
  x_vel <- get_uvel(turb)
  x <- x_vel[,1]   
  x_sk[fl,]<-sk(x, dati) 
  print.hist.gauss(x, path_output, "x", dati[2])
  
  # Finding kurtosis-skewness for y-velocity
  y_vel <- get_vvel(turb)
  y <- y_vel[,1]   
  y_sk[fl,]<-sk(y, dati) 
  print.hist.gauss(y, path_output, "y", dati[2])
  
  # Finding kurtosis-skewness for z-velocity
  z_vel <- get_zvel(turb)
  z <- z_vel[,1]   
  z_sk[fl,]<-sk(z, dati) 
  print.hist.gauss(z, path_output, "z", dati[2])

  
  #Here there is the programme that studies the skewness and kurtosis coefficient of our data
  
  dim_bl <- 300
  # Extracting blocks of 5 minutes from original dataset
  
  time_stamp <- seq(from=0, to=length(z)-1)*(1/sonic_fqc)
  numb <- length(z)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
  # seconds, not in 0.1s...
  cat("* Number of blocks: ",numb,"\n")
  
  m.x_sk <- matrix(ncol = 4 ,nrow = numb)
  m.y_sk <- matrix(ncol = 4 ,nrow = numb)
  m.z_sk <- matrix(ncol = 4 ,nrow = numb)
  
  tempo<-dati
  
  for(block in 1:numb){
    
    tempo[2]<-dati[2] + (block-1)*0.05
    m.x_sk[block,]<- boh(time_stamp, x, block, dim_bl,tempo, numb)
    m.y_sk[block,]<- boh(time_stamp, y, block, dim_bl,tempo, numb)
    m.z_sk[block,]<- boh(time_stamp, z, block, dim_bl,tempo, numb)
  }
  

  
  sk_plot(m.x_sk, paste(path_output, dati[2], "_", sep = ''), "x")
  sk_plot(m.y_sk, paste(path_output, dati[2], "_", sep = ''), "y")
  sk_plot(m.z_sk, paste(path_output, dati[2], "_", sep = ''), "z")
  
}


sk_plot(x_sk, path_output ,"x")
sk_plot(y_sk, path_output, "y")
sk_plot(z_sk, path_output, "z")

sk_plot.xyz(x_sk, y_sk, z_sk, path_output)

