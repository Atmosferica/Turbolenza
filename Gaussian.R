
for(i in 1:length(filename_tot))
{
  
  # Extracted data from csv using the script convert_cvs.awk. 
  # header=TRUE --> Essential! High performance decay for header=FALSE	
  data <- read.csv(filename_tot[i], header=TRUE)
  dati <- read.title.time(filename[i])
     # Converted data (of class data.frame) into an object of class turbulence
     turb <- as.turbulence(data)
     turb <- set_hvel(turb) # setting horizontal velocity
     turb <- set_direction(turb)  # setting direction
     mem<-dati[1]
  }
  
  if (fl!=1){
     data <- read.csv(filename_tot[fl], header=TRUE)
     dati <- read.title.time(filename[fl])
     turb <- as.turbulence(data)
     turb <- set_hvel(turb) # setting horizontal velocity
     turb <- set_direction(turb)  # setting direction
    
             
             # Converted data (of class data.frame) into an object of class turbulence
             

  
  # turb <- set_hvel(turb) # setting horizontal velocity

  #create_directory(name_dir[i])
  cat(name_dir[i],"\n")
             
             # Converted data (of class data.frame) into an object of class turbulence
             

  ##Firt column: skewness; second column: kurtosis
  #Finding kurtosis-skewness for x-velocity
  x_vel <- get_uvel(turb)
  x <- x_vel[,1]   
  x_sk[i,]<-sk(x, dati) 
  
  
  # Finding kurtosis-skewness for y-velocity
  y_vel <- get_vvel(turb)
  y <- y_vel[,1]   
  y_sk[i,]<-sk(y, dati) 
  
  # Finding kurtosis-skewness for z-velocity
  z_vel <- get_zvel(turb)
  z <- z_vel[,1]   
  z_sk[i,]<-sk(z, dati) 

  
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
  
  path_output <- paste('grafici_output/', line, '/',dati[1], '/', sep = '')  
create_directory(path_output)
  path_output <- paste('grafici_output/', line, '/',dati[1], '/', dati[2], '/', sep = '')
  create_directory(path_output)
  
  sk_plot(m.x_sk, path_output,"x")
  sk_plot(m.y_sk, path_output,"y")
  sk_plot(m.z_sk, path_output,"z")
  
}


path_output1 <- paste('grafici_output/', line, '/', dati[1], '/', sep = '')

sk_plot(x_sk, path_output1 ,"x")
sk_plot(y_sk, path_output1, "y")
sk_plot(z_sk, path_output1, "z")