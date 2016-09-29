##This file studies Kurtosis, Skewness and the Normal path of wind speed.

#Define:
x_sk <-matrix(nrow=length(filename_dati_tot), ncol=4)
y_sk <-matrix(nrow=length(filename_dati_tot), ncol=4)
z_sk <-matrix(nrow=length(filename_dati_tot), ncol=4)
h_sk <-matrix(nrow=length(filename_dati_tot), ncol=4)

x_gauss <-matrix(nrow=length(filename_dati_tot), ncol=4)
y_gauss <-matrix(nrow=length(filename_dati_tot), ncol=4)
z_gauss <-matrix(nrow=length(filename_dati_tot), ncol=4)
h_gauss <-matrix(nrow=length(filename_dati_tot), ncol=4)

x_test <-matrix(nrow=length(filename_dati_tot), ncol=3)
y_test <-matrix(nrow=length(filename_dati_tot), ncol=3)
z_test <-matrix(nrow=length(filename_dati_tot), ncol=3)
h_test <-matrix(nrow=length(filename_dati_tot), ncol=3)


n<-0


 f_cut_up <- sonic_fqc/2
f_cut_down_z <- 0.035337
f_cut_down_x <- 0.022385
f_cut_down_y <- 0.022385

#Here the cycle starts: it reads all the files.Inside this cycle, there is
#another for cycle that works C
for(fl in 1:length(filename_dati_tot)){
  # # Finding kurtosis-skewness for h-velocity
  if (fl==1) {
     # Extracted data from csv using the script convert_cvs.awk. 
     # header=TRUE --> Essential! High performance decay for header=FALSE	
     data <- read.csv(filename_dati_tot[fl], header=TRUE)
     dati <- read.title.time(filename_dati[fl])

     mem<-dati[1]
  }
  
  if (fl!=1){
     data <- read.csv(filename_dati_tot[fl], header=TRUE)
     dati <- read.title.time(filename_dati[fl])

     if (dati[1]!=mem)  {
       
        for(counter in (n+1):(fl-1))  {

          print(counter)
             data <- read.csv(filename_dati_tot[counter], header=TRUE)
             info <- read.title.time(filename_dati[counter])
             
             # Converted data (of class data.frame) into an object of class turbulence
              turb <- as.turbulence(data)
              turb <- set_hvel(turb) # setting horizontal velocity
              turb <- set_direction(turb)  # setting direction

              cat(name_dir[counter],"\n")
              path_output <- paste('grafici_output/', line, '/',info[1], '/con_filtro/', sep = '')  
              create_directory(path_output)
             
              path_output_new<-paste(path_output, "block/", sep ='')
              create_directory(path_output_new)
             ##Finding kurtosis-skewness and mean-sd for x-velocity.
             ##firth column: date; second : hour
             ##third column: skewness; fourth column: kurtosis
             ##third column: mean; fourth column: sd
              x_vel <- get_uvel(turb)
              x <- x_vel[,1]  
             
              hamming <- hamming.window(length(x))
              hamming <- hamming/sum(hamming)*length(x)
              x <- x*hamming
              fft_x<- dofft(x,sonic_fqc)
              down_smooth<-LowPassfilter.data(fft_x$freq,fft_x$fft_vel,f_cut_up)
              tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down_x)
              x <- Re(tot_smooth$vel)/hamming 
             
              x_sk[counter,]<-sk(x, info)
              x_test[counter, ]<-normality.score(x_sk[counter,],info)
              x_gauss[counter, ]<-gauss(x, info)
              print.hist.gauss(x, path_output, "x", info[2])
 
             ##Finding kurtosis-skewness and mean-sd for y-velocity.
             ##firth column: date; second : hour
             ##third column: skewness; fourth column: kurtosis
             ##third column: mean; fourth column: sd
              y_vel <- get_vvel(turb)
              y <- y_vel[,1]   
              
              
              down_smooth<-NULL
              tot_smooth<-NULL
              hamming <- hamming.window(length(y))
              hamming <- hamming/sum(hamming)*length(y)
              y <- y*hamming
              fft_y<- dofft(y,sonic_fqc)
              down_smooth<-LowPassfilter.data(fft_y$freq,fft_y$fft_vel,f_cut_up)
              tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down_y)
              y <- Re(tot_smooth$vel)/hamming 
              
              y_sk[counter,]<-sk(y,info) 
              y_gauss[counter, ]<-gauss(y, info)
              y_test[counter, ]<-normality.score(y_sk[counter,],info)
              print.hist.gauss(y, path_output, "y", info[2])

              ##Finding kurtosis-skewness and mean-sd for z-velocity.
              ##firth column: date; second : hour
              ##third column: skewness; fourth column: kurtosis
              ##third column: mean; fourth column: sd
              z_vel <- get_zvel(turb)
              z <- z_vel[,1]
              
              down_smooth<-NULL
              tot_smooth<-NULL
              hamming <- hamming.window(length(z))
              hamming <- hamming/sum(hamming)*length(z)
              z <- z*hamming
              fft_z<- dofft(z,sonic_fqc)
              down_smooth<-LowPassfilter.data(fft_z$freq,fft_z$fft_vel,f_cut_up)
              tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down_z)
              z <- Re(tot_smooth$vel)/hamming 
              
              z_sk[counter,]<-sk(z, info)
              z_gauss[counter, ]<-gauss(z, info)
              z_test[counter, ]<-normality.score(z_sk[counter,],info)
              print.hist.gauss(z, path_output, "z", info[2])

              ##Finding kurtosis-skewness and mean-sd for h-velocity.
              ##firth column: date; second : hour
              ##third column: skewness; fourth column: kurtosis
              ##third column: mean; fourth column: sd             

              h_vel <- get_hvel(turb)
              h <- h_vel[,1]
              
              h_sk[counter,]<-sk(h, info)
              h_gauss[counter, ]<-gauss(h, info)
              h_test[counter, ]<-normality.score(h_sk[counter,],info)
              print.hist.gauss(h, path_output, "h", info[2])

              ##Finding kurtosis-skewness and mean-sd for velocity direction.
              ##firth column: date; second : hour
              ##third column: mean; fourth column: sd   
              theta <- get_direction(turb)
              print.hist.gauss(theta, path_output, "theta", info[2])


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
             
             m.x_gauss <- matrix(ncol = 4 ,nrow = numb)
             m.y_gauss <- matrix(ncol = 4 ,nrow = numb)
             m.z_gauss <- matrix(ncol = 4 ,nrow = numb)
             m.h_gauss <- matrix(ncol = 4 ,nrow = numb)
             
             m.x_test <- matrix(ncol = 3 ,nrow = numb)
             m.y_test <- matrix(ncol = 3 ,nrow = numb)
             m.z_test <- matrix(ncol = 3 ,nrow = numb)
             m.h_test <- matrix(ncol = 3 ,nrow = numb)
             
             

             tempo<-info
             for(block in 1:numb){
               tempo[2]<-info[2] + (block-1)*0.05
               m.x_sk[block,]<- sk.blocks(time_stamp, x, block, dim_bl,tempo)
               m.y_sk[block,]<- sk.blocks(time_stamp, y, block, dim_bl,tempo)
               m.z_sk[block,]<- sk.blocks(time_stamp, z, block, dim_bl,tempo)
               m.h_sk[block,]<- sk.blocks(time_stamp, h, block, dim_bl,tempo)
               
               m.x_gauss[block,] <-gauss.blocks(time_stamp, x, block, dim_bl, tempo)
               m.y_gauss[block,] <-gauss.blocks(time_stamp, y, block, dim_bl, tempo)
               m.z_gauss[block,] <-gauss.blocks(time_stamp, z, block, dim_bl, tempo)
               m.h_gauss[block,] <-gauss.blocks(time_stamp, h, block, dim_bl, tempo)
               
               m.x_test[block, ]<-normality.score(m.x_sk[block,],tempo)
               m.y_test[block, ]<-normality.score(m.y_sk[block,],tempo)
               m.z_test[block, ]<-normality.score(m.z_sk[block,],tempo)
               m.h_test[block, ]<-normality.score(m.h_sk[block,],tempo)
               
               printBlock.hist.gauss(x, path_output_new, "x", block, dim_bl, tempo[2])
               printBlock.hist.gauss(y, path_output_new, "y", block, dim_bl, tempo[2])
               printBlock.hist.gauss(z, path_output_new, "z", block, dim_bl, tempo[2])
               printBlock.hist.gauss(h, path_output_new, "h", block, dim_bl, tempo[2])
               printBlock.hist.gauss(theta, path_output_new, "theta", block, dim_bl, tempo[2])
             
             }
          
             sk_plot(m.x_sk, paste(path_output_new, info[2], "_", sep = ''), "x")
             sk_plot(m.y_sk, paste(path_output_new, info[2], "_", sep = ''), "y")
             sk_plot(m.z_sk, paste(path_output_new, info[2], "_", sep = ''), "z")
             sk_plot(m.h_sk, paste(path_output_new, info[2], "_", sep = ''), "h")
             
             gauss_plot(m.x_gauss, paste(path_output_new, info[2], "_", sep = '') ,"x")
             gauss_plot(m.y_gauss, paste(path_output_new, info[2], "_", sep = '') ,"y")
             gauss_plot(m.z_gauss, paste(path_output_new, info[2], "_", sep = '') ,"z")
             gauss_plot(m.h_gauss, paste(path_output_new, info[2], "_", sep = '') ,"h")
             
             norm.test_plot(m.x_test, paste(path_output_new, info[2], "_", sep = '') ,"x")
             norm.test_plot(m.y_test, paste(path_output_new, info[2], "_", sep = '') ,"y")
             norm.test_plot(m.z_test, paste(path_output_new, info[2], "_", sep = '') ,"z")
             norm.test_plot(m.h_test, paste(path_output_new, info[2], "_", sep = '') ,"h")
             
        }
       

       
       sk_plot(x_sk[(n+1):counter, ], path_output ,"x")
       sk_plot(y_sk[(n+1):counter, ], path_output, "y")
       sk_plot(z_sk[(n+1):counter, ], path_output, "z")
       sk_plot(h_sk[(n+1):counter, ], path_output, "h")

       gauss_plot(x_gauss[(n+1):counter, ], path_output ,"x")
       gauss_plot(y_gauss[(n+1):counter, ], path_output ,"y")
       gauss_plot(z_gauss[(n+1):counter, ], path_output ,"z")
       gauss_plot(h_gauss[(n+1):counter, ], path_output ,"h")
       
       norm.test_plot(x_test[(n+1):counter, ], path_output ,"x")
       norm.test_plot(y_test[(n+1):counter, ], path_output ,"y")
       norm.test_plot(z_test[(n+1):counter, ], path_output ,"z")
       norm.test_plot(h_test[(n+1):counter, ], path_output ,"h")
       
       sk_plot.xyz(x_sk[(n+1):counter, ], y_sk[(n+1):counter, ], z_sk[(n+1):counter, ], path_output)
       n <- counter
    
     }
     if (fl==length(filename_dati_tot)) {

        for(counter in (n+1):fl)  {

          data <- read.csv(filename_dati_tot[counter], header=TRUE)
          info <- read.title.time(filename_dati[counter])
          
          turb <- as.turbulence(data)
          turb <- set_hvel(turb) # setting horizontal velocity
          turb <- set_direction(turb)  # setting direction
          
          cat(name_dir[counter],"\n")
          path_output <- paste('grafici_output/', line, '/',info[1], '/con_filtro/', sep = '')
          create_directory(path_output)
          
          path_output_new<-paste(path_output, "block/", sep ='')
          create_directory(path_output_new)

          ##Finding kurtosis-skewness and mean-sd for x-velocity.
          ##firth column: date; second : hour
          ##third column: skewness; fourth column: kurtosis
          ##third column: mean; fourth column: sd
          x_vel <- get_uvel(turb)
          x <- x_vel[,1]  
          
          hamming <- hamming.window(length(x))
          hamming <- hamming/sum(hamming)*length(x)
          x <- x*hamming
          fft_x<- dofft(x,sonic_fqc)
          down_smooth<-LowPassfilter.data(fft_x$freq,fft_x$fft_vel,f_cut_up)
          tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down_x)
          x <- Re(tot_smooth$vel)/hamming 
          
          x_sk[counter,]<-sk(x, info)
          x_test[counter, ]<-normality.score(x_sk[counter,],info)
          x_gauss[counter, ]<-gauss(x, info)
          print.hist.gauss(x, path_output, "x", info[2])

          ##Finding kurtosis-skewness and mean-sd for y-velocity.
          ##firth column: date; second : hour
          ##third column: skewness; fourth column: kurtosis
          ##third column: mean; fourth column: sd
          y_vel <- get_vvel(turb)
          y <- y_vel[,1]   
          
          
          down_smooth<-NULL
          tot_smooth<-NULL
          hamming <- hamming.window(length(y))
          hamming <- hamming/sum(hamming)*length(y)
          y <- y*hamming
          fft_y<- dofft(y,sonic_fqc)
          down_smooth<-LowPassfilter.data(fft_y$freq,fft_y$fft_vel,f_cut_up)
          tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down_y)
          y <- Re(tot_smooth$vel)/hamming 
          
          y_sk[counter,]<-sk(y,info)
          y_test[counter, ]<-normality.score(y_sk[counter,],info)
          y_gauss[counter, ]<-gauss(y, info)
          print.hist.gauss(y, path_output, "y", info[2])

          ##Finding kurtosis-skewness and mean-sd for z-velocity.
          ##firth column: date; second : hour
          ##third column: skewness; fourth column: kurtosis
          ##third column: mean; fourth column: sd
          z_vel <- get_zvel(turb)
          z <- z_vel[,1]
          
          down_smooth<-NULL
          tot_smooth<-NULL
          hamming <- hamming.window(length(z))
          hamming <- hamming/sum(hamming)*length(z)
          z <- z*hamming
          fft_z<- dofft(z,sonic_fqc)
          down_smooth<-LowPassfilter.data(fft_z$freq,fft_z$fft_vel,f_cut_up)
          tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down_z)
          z <- Re(tot_smooth$vel)/hamming 
          
          z_sk[counter,]<-sk(z, info)
          z_test[counter, ]<-normality.score(z_sk[counter,],info)
          z_gauss[counter, ]<-gauss(z, info)
          print.hist.gauss(z, path_output, "z", info[2])

          ##Finding kurtosis-skewness and mean-sd for h-velocity.
          ##firth column: date; second : hour
          ##third column: skewness; fourth column: kurtosis
          ##third column: mean; fourth column: sd             
          h_vel <- get_hvel(turb)
          h <- h_vel[,1]

          h_sk[counter,]<-sk(h, info)
          h_test[counter, ]<-normality.score(h_sk[counter,],info)
          h_gauss[counter, ]<-gauss(h, info)
          print.hist.gauss(h, path_output, "h", info[2])

          ##Finding kurtosis-skewness and mean-sd for velocity direction.
          ##firth column: date; second : hour
          ##third column: mean; fourth column: sd   
          theta <- get_direction(turb)
          print.hist.gauss(theta, path_output, "theta", info[2])

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

          m.x_gauss <- matrix(ncol = 4 ,nrow = numb)
          m.y_gauss <- matrix(ncol = 4 ,nrow = numb)
          m.z_gauss <- matrix(ncol = 4 ,nrow = numb)
          m.h_gauss <- matrix(ncol = 4 ,nrow = numb)
          
          m.x_test <- matrix(ncol = 3 ,nrow = numb)
          m.y_test <- matrix(ncol = 3 ,nrow = numb)
          m.z_test <- matrix(ncol = 3 ,nrow = numb)
          m.h_test <- matrix(ncol = 3 ,nrow = numb)
          
          
          tempo<-info
          for(block in 1:numb){
            tempo[2]<-info[2] + (block-1)*0.05
            m.x_sk[block,]<- sk.blocks(time_stamp, x, block, dim_bl,tempo)
            m.y_sk[block,]<- sk.blocks(time_stamp, y, block, dim_bl,tempo)
            m.z_sk[block,]<- sk.blocks(time_stamp, z, block, dim_bl,tempo)
            m.h_sk[block,]<- sk.blocks(time_stamp, h, block, dim_bl,tempo)

            m.x_gauss[block,] <-gauss.blocks(time_stamp, x, block, dim_bl, tempo)
            m.y_gauss[block,] <-gauss.blocks(time_stamp, y, block, dim_bl, tempo)
            m.z_gauss[block,] <-gauss.blocks(time_stamp, z, block, dim_bl, tempo)
            m.h_gauss[block,] <-gauss.blocks(time_stamp, h, block, dim_bl, tempo)
            
            m.x_test[block, ]<-normality.score(m.x_sk[block,],tempo)
            m.y_test[block, ]<-normality.score(m.y_sk[block,],tempo)
            m.z_test[block, ]<-normality.score(m.z_sk[block,],tempo)
            m.h_test[block, ]<-normality.score(m.h_sk[block,],tempo)
            

            printBlock.hist.gauss(x, path_output_new, "x", block, dim_bl, tempo[2])
            printBlock.hist.gauss(y, path_output_new, "y", block, dim_bl, tempo[2])
            printBlock.hist.gauss(z, path_output_new, "z", block, dim_bl, tempo[2])
            printBlock.hist.gauss(h, path_output_new, "h", block, dim_bl, tempo[2])
            printBlock.hist.gauss(theta, path_output_new, "theta", block, dim_bl, tempo[2])
            
          }

#plot per blocchetto                    
          sk_plot(m.x_sk, paste(path_output_new, info[2], "_", sep = ''), "x")
          sk_plot(m.y_sk, paste(path_output_new, info[2], "_", sep = ''), "y")
          sk_plot(m.z_sk, paste(path_output_new, info[2], "_", sep = ''), "z")
          sk_plot(m.h_sk, paste(path_output_new, info[2], "_", sep = ''), "h")
          
          gauss_plot(m.x_gauss, paste(path_output_new, info[2], "_", sep = '') ,"x")
          gauss_plot(m.y_gauss, paste(path_output_new, info[2], "_", sep = '') ,"y")
          gauss_plot(m.z_gauss, paste(path_output_new, info[2], "_", sep = '') ,"z")
          gauss_plot(m.h_gauss, paste(path_output_new, info[2], "_", sep = '') ,"h")
          
          norm.test_plot(m.x_test, paste(path_output_new, info[2], "_", sep = '') ,"x")
          norm.test_plot(m.y_test, paste(path_output_new, info[2], "_", sep = '') ,"y")
          norm.test_plot(m.z_test, paste(path_output_new, info[2], "_", sep = '') ,"z")
          norm.test_plot(m.h_test, paste(path_output_new, info[2], "_", sep = '') ,"h")
          
  
        } 
#plot orari
       sk_plot(x_sk[(n+1):counter, ], path_output ,"x")
       sk_plot(y_sk[(n+1):counter, ], path_output, "y")
       sk_plot(z_sk[(n+1):counter, ], path_output, "z")
       sk_plot(h_sk[(n+1):counter, ], path_output, "h")
       
       gauss_plot(x_gauss[(n+1):counter, ], path_output ,"x")
       gauss_plot(y_gauss[(n+1):counter, ], path_output ,"y")
       gauss_plot(z_gauss[(n+1):counter, ], path_output ,"z")
       gauss_plot(h_gauss[(n+1):counter, ], path_output ,"h")
       
       norm.test_plot(x_test[(n+1):counter, ], path_output ,"x")
       norm.test_plot(y_test[(n+1):counter, ], path_output ,"y")
       norm.test_plot(z_test[(n+1):counter, ], path_output ,"z")
       norm.test_plot(h_test[(n+1):counter, ], path_output ,"h")
       

       sk_plot.xyz(x_sk[(n+1):counter, ], y_sk[(n+1):counter, ], z_sk[(n+1):counter, ], path_output)

       n <- counter
       
     }

     mem<-dati[1]
  }
  
  }

   
  
