##This file studies Kurtosis, Skewness and the Normal path of wind speed.

#Define:
x_sk <-matrix(nrow=length(filename_tot), ncol=4)
y_sk <-matrix(nrow=length(filename_tot), ncol=4)
z_sk <-matrix(nrow=length(filename_tot), ncol=4)
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
     # Converted data (of class data.frame) into an object of class turbulence
     turb <- as.turbulence(data)
     mem<-dati[1]
  }
  
  if (fl!=1){
     data <- read.csv(filename_tot[fl], header=TRUE)
     dati <- read.title.time(filename[fl])
     turb <- as.turbulence(data)
    
     if (dati[1]!=mem)  {
       
        for(counter in (n+1):(fl-1))  {
             print (counter)
             data <- read.csv(filename_tot[counter], header=TRUE)
             info <- read.title.time(filename[counter])
             turb <- as.turbulence(data)
             turb <- set_direction(turb)
             cat(name_dir[counter],"\n")
             path_output <- paste('grafici_output/', line, '/',info[1], '/', sep = '')  
             create_directory(path_output)
         
             ##Finding kurtosis-skewness for x-velocity. 
             ##Firt column: skewness; second column: kurtosis
             x_vel <- get_uvel(turb)
             x <- x_vel[,1]   
             x_sk[counter,]<-sk(x, info) 
             print.hist.gauss(x, path_output, "x", info[2])
         
             # # Finding kurtosis-skewness for y-velocity
              y_vel <- get_vvel(turb)
              y <- y_vel[,1]   
              y_sk[counter,]<-sk(y,info) 
              print.hist.gauss(y, path_output, "y", info[2])
              
             # # Finding kurtosis-skewness for z-velocity
              z_vel <- get_zvel(turb)
              z <- z_vel[,1]   
              z_sk[counter,]<-sk(z, info) 
              print.hist.gauss(z, path_output, "z", info[2])
         
             #Here there is the programme that studies the skewness and kurtosis coefficient of our data
             # Extracting blocks of 5 minutes from original dataset
             dim_bl <- 300
             time_stamp <- seq(from=0, to=length(z)-1)*(1/sonic_fqc)
             numb <- length(z)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
             # seconds, not in 0.1s...
             cat("* Number of blocks: ",numb,"\n")
          
             #Creating matrices with 4 columns:
             #1: dat, 2: blocco, 3: skewness, 4: Kurtosis
             m.x_sk <- matrix(ncol = 4 ,nrow = numb)
             m.y_sk <- matrix(ncol = 4 ,nrow = numb)
             m.z_sk <- matrix(ncol = 4 ,nrow = numb)
          
             tempo<-info
             for(block in 1:numb){
               tempo[2]<-info[2] + (block-1)*0.05
               m.x_sk[block,]<- boh(time_stamp, x, block, dim_bl,tempo, numb)
               m.y_sk[block,]<- boh(time_stamp, y, block, dim_bl,tempo, numb)
               m.z_sk[block,]<- boh(time_stamp, z, block, dim_bl,tempo, numb)
             }
          
             sk_plot(m.x_sk, paste(path_output, info[2], "_", sep = ''), "x")
             sk_plot(m.y_sk, paste(path_output, info[2], "_", sep = ''), "y")
             sk_plot(m.z_sk, paste(path_output, info[2], "_", sep = ''), "z")
        }
       
           n <- counter
           sk_plot(x_sk, path_output ,"x")
           sk_plot(y_sk, path_output, "y")
           sk_plot(z_sk, path_output, "z")
           #sk_plot.xyz(x_sk, y_sk, z_sk, path_output)
    
     }
     if (fl==length(filename_tot)) {
       
       for(counter in (n+1):fl)  {
         print (counter)
         data <- read.csv(filename_tot[counter], header=TRUE)
         info <- read.title.time(filename[counter])
         turb <- as.turbulence(data)
         turb <- set_direction(turb)
         cat(name_dir[counter],"\n")
         path_output <- paste('grafici_output/', line, '/',info[1], '/', sep = '')  
         create_directory(path_output)
         
         ##Finding kurtosis-skewness for x-velocity. 
         ##Firt column: skewness; second column: kurtosis
         x_vel <- get_uvel(turb)
         x <- x_vel[,1]   
         x_sk[counter,]<-sk(x, info) 
         print.hist.gauss(x, path_output, "x", info[2])
         
         # # Finding kurtosis-skewness for y-velocity
         y_vel <- get_vvel(turb)
         y <- y_vel[,1]   
         y_sk[counter,]<-sk(y,info) 
         print.hist.gauss(y, path_output, "y", info[2])
         
         # # Finding kurtosis-skewness for z-velocity
         z_vel <- get_zvel(turb)
         z <- z_vel[,1]   
         z_sk[counter,]<-sk(z, info) 
         print.hist.gauss(z, path_output, "z", info[2])
         
         #Here there is the programme that studies the skewness and kurtosis coefficient of our data
         # Extracting blocks of 5 minutes from original dataset
         dim_bl <- 300
         time_stamp <- seq(from=0, to=length(z)-1)*(1/sonic_fqc)
         numb <- length(z)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
         # seconds, not in 0.1s...
         cat("* Number of blocks: ",numb,"\n")
         
         #Creating matrices with 4 columns:
         #1: dat, 2: blocco, 3: skewness, 4: Kurtosis
         m.x_sk <- matrix(ncol = 4 ,nrow = numb)
         m.y_sk <- matrix(ncol = 4 ,nrow = numb)
         m.z_sk <- matrix(ncol = 4 ,nrow = numb)
         
         tempo<-info
         for(block in 1:numb){
           tempo[2]<-info[2] + (block-1)*0.05
           m.x_sk[block,]<- boh(time_stamp, x, block, dim_bl,tempo, numb)
           m.y_sk[block,]<- boh(time_stamp, y, block, dim_bl,tempo, numb)
           m.z_sk[block,]<- boh(time_stamp, z, block, dim_bl,tempo, numb)
         }
         
         sk_plot(m.x_sk, paste(path_output, info[2], "_", sep = ''), "x")
         sk_plot(m.y_sk, paste(path_output, info[2], "_", sep = ''), "y")
         sk_plot(m.z_sk, paste(path_output, info[2], "_", sep = ''), "z")
       }
       
       n <- counter
       sk_plot(x_sk, path_output ,"x")
       sk_plot(y_sk, path_output, "y")
       sk_plot(z_sk, path_output, "z")
       #sk_plot.xyz(x_sk, y_sk, z_sk, path_output)
     }
     
     mem<-dati[1]
  }
  
}

   
  