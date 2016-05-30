
#Define:
x_test <-matrix(nrow=length(filename_dati_tot), ncol=4)
y_test <-matrix(nrow=length(filename_dati_tot), ncol=4)
z_test <-matrix(nrow=length(filename_dati_tot), ncol=4)
h_test <-matrix(nrow=length(filename_dati_tot), ncol=4)

n<-0

f_cut_up <- sonic_fqc/2
f_cut_down <- 0.01048

#Here the cycle starts: it reads all the files.Inside this cycle, there is
#another for cycle that works C
for(fl in 1:length(filename_dati_tot))
{
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
        
        ##Performing Kolmogorov-Smirnoff test.
        ##firth column: date; second : hour
        ##third column: d ; fourth column: p-value
        #u
        x_vel <- get_uvel(turb)
        x <- x_vel[,1]  
        
        #Filtering
        hamming <- hamming.window(length(x))
        hamming <- hamming/sum(hamming)*length(x)
        x <- x*hamming
        fft_x<- dofft(x,sonic_fqc)
        down_smooth<-LowPassfilter.data(fft_x$freq,fft_x$fft_vel,f_cut_up)
        tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
        x <- Re(tot_smooth$vel)/hamming 
        
        #test
        x_test[counter,]<-kolm.test(x, info)
        
        #v
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

        y_test[counter,]<-kolm.test(y, info)

        #w
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
        
        z_test[counter,]<-kolm.test(z, info)
        
        #h
        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        
        h_test[counter,]<-kolm.test(h, info)

        
        
         # Extracting blocks of 5 minutes from original dataset
        dim_bl <- 300
        time_stamp <- seq(from=0, to=length(z)-1)*(1/sonic_fqc)
        numb <- length(z)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
        # seconds, not in 0.1s...
        cat("* Number of blocks: ",numb,"\n")
        
        #Creating matrices with 4 columns:
        
        m.x_test <- matrix(ncol = 4 ,nrow = numb)
        m.y_test <- matrix(ncol = 4 ,nrow = numb)
        m.z_test <- matrix(ncol = 4 ,nrow = numb)
        m.h_test <- matrix(ncol = 4 ,nrow = numb)
        
        
        tempo<-info
        for(block in 1:numb){
          tempo[2]<-info[2] + (block-1)*0.05

          m.x_test[block,] <-kolm.test.blocks(time_stamp, x, block, dim_bl, tempo)
          m.y_test[block,] <-kolm.test.blocks(time_stamp, y, block, dim_bl, tempo)
          m.z_test[block,] <-kolm.test.blocks(time_stamp, z, block, dim_bl, tempo)
          m.h_test[block,] <-kolm.test.blocks(time_stamp, h, block, dim_bl, tempo)


          
        }
  
        
        kolm.test_plot(m.x_gauss, paste(path_output_new, info[2], "_", sep = '') ,"x")
        kolm.test_plot(m.y_gauss, paste(path_output_new, info[2], "_", sep = '') ,"y")
        kolm.test_plot(m.z_gauss, paste(path_output_new, info[2], "_", sep = '') ,"z")
        kolm.test_plot(m.h_gauss, paste(path_output_new, info[2], "_", sep = '') ,"h")
      }
      
      
      kolm.test_plot(x_test[(n+1):counter, ], path_output ,"x")
      kolm.test_plot(y_test[(n+1):counter, ], path_output ,"y")
      kolm.test_plot(z_test[(n+1):counter, ], path_output ,"z")
      kolm.test_plot(h_test[(n+1):counter, ], path_output ,"h") 
      
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
        
        ##Performing Kolmogorov-Smirnoff test.
        ##firth column: date; second : hour
        ##third column: d ; fourth column: p-value
        #u
        x_vel <- get_uvel(turb)
        x <- x_vel[,1]  
        
        hamming <- hamming.window(length(x))
        hamming <- hamming/sum(hamming)*length(x)
        x <- x*hamming
        fft_x<- dofft(x,sonic_fqc)
        down_smooth<-LowPassfilter.data(fft_x$freq,fft_x$fft_vel,f_cut_up)
        tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
        x <- Re(tot_smooth$vel)/hamming 
        
        x_test[counter,]<-kolm.test(x, info)
        
        #v
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
        
        y_test[counter,]<-kolm.test(y, info)
        
        #w
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

        z_test[counter,]<-kolm.test(z, info)

        #h           
        h_vel <- get_hvel(turb)
        h <- h_vel[,1]
        
        h_test[counter,]<-kolm.test(h, info)
        

        
        #Here there is the programme that studies the skewness and kurtosis coefficient of our data
        # Extracting blocks of 5 minutes from original dataset
        dim_bl <- 300
        time_stamp <- seq(from=0, to=length(z)-1)*(1/sonic_fqc)
        numb <- length(z)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
        # seconds, not in 0.1s...
        cat("* Number of blocks: ",numb,"\n")
        
        m.x_test <- matrix(ncol = 4 ,nrow = numb)
        m.y_test <- matrix(ncol = 4 ,nrow = numb)
        m.z_test <- matrix(ncol = 4 ,nrow = numb)
        m.h_test <- matrix(ncol = 4 ,nrow = numb)
        
        
        tempo<-info
        for(block in 1:numb){
          tempo[2]<-info[2] + (block-1)*0.05

          m.x_test[block,] <-kolm.test.blocks(time_stamp, x, block, dim_bl, tempo)
          m.y_test[block,] <-kolm.test.blocks(time_stamp, y, block, dim_bl, tempo)
          m.z_test[block,] <-kolm.test.blocks(time_stamp, z, block, dim_bl, tempo)
          m.h_test[block,] <-kolm.test.blocks(time_stamp, h, block, dim_bl, tempo)
          
        }
        
        #plot per blocchetto                    
        kolm.test_plot(m.x_test, paste(path_output_new, info[2], "_", sep = '') ,"x")
        kolm.test_plot(m.y_test, paste(path_output_new, info[2], "_", sep = '') ,"y")
        kolm.test_plot(m.z_test, paste(path_output_new, info[2], "_", sep = '') ,"z")
        kolm.test_plot(m.h_test, paste(path_output_new, info[2], "_", sep = '') ,"h")
      }
      
      #plot orari
      kolm.test_plot(x_test[(n+1):counter, ], path_output ,"x")
      kolm.test_plot(y_test[(n+1):counter, ], path_output ,"y")
      kolm.test_plot(z_test[(n+1):counter, ], path_output ,"z")
      kolm.test_plot(h_test[(n+1):counter, ], path_output ,"h") 
      
      n <- counter
      
    }
    
    mem<-dati[1]
  }
  
}