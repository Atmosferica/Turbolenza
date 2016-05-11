##This file studies Kurtosis, Skewness and the Normal path of wind speed.

#Define:
x_sk <-matrix(nrow=length(filename_tot), ncol=4)
y_sk <-matrix(nrow=length(filename_tot), ncol=4)
z_sk <-matrix(nrow=length(filename_tot), ncol=4)
h_sk <-matrix(nrow=length(filename_tot), ncol=4)

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
             print (counter)
             data <- read.csv(filename_tot[counter], header=TRUE)
             info <- read.title.time(filename[counter])
             
             # Converted data (of class data.frame) into an object of class turbulence
             turb <- as.turbulence(data)
             turb <- set_hvel(turb) # setting horizontal velocity
             turb <- set_direction(turb)  # setting direction
             
             cat(name_dir[counter],"\n")
             path_output <- paste('grafici_output/', line, '/',info[1], '/', sep = '')  
             create_directory(path_output)
         
             ##Finding kurtosis-skewness for x-velocity. 
             ##Firt column: skewness; second column: kurtosis
             x_vel <- get_uvel(turb)
             x <- x_vel[,1]

	     f_cut_up <- 0.015
	     f_cut_down <- 0.005

	     hamming <- hamming.window(length(x))
	     hamming <- hamming/sum(hamming)*length(x)
	     x <- x*hamming
	     fft_x<- dofft(x,sonic_fqc)
	     down_smooth<-LowPassfilter.data(fft_x$freq,fft_x$fft_vel,f_cut_up)
	     tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
	     x <- Re(tot_smooth$vel)/hamming 


             x_sk[counter,]<-sk(x, info) 
             print.hist.gauss(x, path_output, "x", info[2])
         
             # # Finding kurtosis-skewness for y-velocity
             y_vel <- get_vvel(turb)
             y <- y_vel[,1]   
	     
	     down_smooth<-NULL
	     tot_smooth<-NULL
	     hamming <- hamming.window(length(y))
	     hamming <- hamming/sum(hamming)*length(y)
	     y <- y*hamming
	     fft_y<- dofft(y,sonic_fqc)
	     down_smooth<-LowPassfilter.data(fft_y$freq,fft_y$fft_vel,f_cut_up)
	     tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
	     y <- Re(tot_smooth$vel)/hamming 

	     y_sk[counter,]<-sk(y,info) 
             print.hist.gauss(y, path_output, "y", info[2])
              
             # # Finding kurtosis-skewness for z-velocity
             z_vel <- get_zvel(turb)
             z <- z_vel[,1]

	     down_smooth<-NULL
	     tot_smooth<-NULL
	     hamming <- hamming.window(length(z))
	     hamming <- hamming/sum(hamming)*length(z)
	     z <- z*hamming
	     fft_z<- dofft(z,sonic_fqc)
	     down_smooth<-LowPassfilter.data(fft_z$freq,fft_z$fft_vel,f_cut_up)
	     tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
	     z <- Re(tot_smooth$vel)/hamming 

             z_sk[counter,]<-sk(z, info)
             print.hist.gauss(z, path_output, "z", info[2])
              
             # # Finding kurtosis-skewness for h-velocity
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
 
	     h_sk[counter,]<-sk(h, info)
             print.hist.gauss(h, path_output, "h", info[2])
         
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
             m.h_sk <- matrix(ncol = 4 ,nrow = numb)
          
             tempo<-info
             for(block in 1:numb){
               tempo[2]<-info[2] + (block-1)*0.05
               m.x_sk[block,]<- sk.blocks(time_stamp, x, block, dim_bl,tempo)
               m.y_sk[block,]<- sk.blocks(time_stamp, y, block, dim_bl,tempo)
               m.z_sk[block,]<- sk.blocks(time_stamp, z, block, dim_bl,tempo)
               m.h_sk[block,]<- sk.blocks(time_stamp, h, block, dim_bl,tempo)
             }
          
             sk_plot(m.x_sk, paste(path_output, info[2], "_", sep = ''), "x")
             sk_plot(m.y_sk, paste(path_output, info[2], "_", sep = ''), "y")
             sk_plot(m.z_sk, paste(path_output, info[2], "_", sep = ''), "z")
             sk_plot(m.h_sk, paste(path_output, info[2], "_", sep = ''), "h")
        }
       

       sk_plot(x_sk[(n+1):counter, ], path_output ,"x")
       sk_plot(y_sk[(n+1):counter, ], path_output, "y")
       sk_plot(z_sk[(n+1):counter, ], path_output, "z")
       sk_plot(h_sk[(n+1):counter, ], path_output, "h")
       sk_plot.xyzh(x_sk[(n+1):counter, ], y_sk[(n+1):counter, ], z_sk[(n+1):counter, ], h_sk[(n+1):counter, ], path_output)
       n <- counter
    
     }
     if (fl==length(filename_tot)) {

        for(counter in (n+1):fl)  {
          print (counter)
          data <- read.csv(filename_tot[counter], header=TRUE)
          info <- read.title.time(filename[counter])
          turb <- as.turbulence(data)
          turb <- set_direction(turb)
          turb <- set_hvel(turb) # setting horizontal velocity
          turb <- set_direction(turb)  # setting direction
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
          
          # # Finding kurtosis-skewness for h-velocity
          h_vel <- get_hvel(turb)
          h <- h_vel[,1]
          h_sk[counter,]<-sk(h, info)
          print.hist.gauss(h, path_output, "h", info[2])

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
          m.h_sk <- matrix(ncol = 4 ,nrow = numb)

          tempo<-info
          for(block in 1:numb){
            tempo[2]<-info[2] + (block-1)*0.05
            m.x_sk[block,]<- sk.blocks(time_stamp, x, block, dim_bl,tempo)
            m.y_sk[block,]<- sk.blocks(time_stamp, y, block, dim_bl,tempo)
            m.z_sk[block,]<- sk.blocks(time_stamp, z, block, dim_bl,tempo)
            m.h_sk[block,]<- sk.blocks(time_stamp, h, block, dim_bl,tempo)
          }

          sk_plot(m.x_sk, paste(path_output, info[2], "_", sep = ''), "x")
          sk_plot(m.y_sk, paste(path_output, info[2], "_", sep = ''), "y")
          sk_plot(m.z_sk, paste(path_output, info[2], "_", sep = ''), "z")
          sk_plot(m.h_sk, paste(path_output, info[2], "_", sep = ''), "h")
        }


       
       sk_plot(x_sk[(n+1):counter, ], path_output ,"x")
       sk_plot(y_sk[(n+1):counter, ], path_output, "y")
       sk_plot(z_sk[(n+1):counter, ], path_output, "z")
       sk_plot(h_sk[(n+1):counter, ], path_output, "h")
       sk_plot.xyzh(x_sk[(n+1):counter, ], y_sk[(n+1):counter, ], z_sk[(n+1):counter, ], h_sk[(n+1):counter, ], path_output)
       n <- counter
       
      }
     mem<-dati[1]
  }
  
}

   
  
