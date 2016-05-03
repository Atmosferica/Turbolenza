# ******* Markov Test ***************

markov_path <- paste(name_dir[i], '/markov', sep='')
create_directory(markov_path)
dim_bl <- 300
# Extracting blocks of 5 minutes from original dataset

f_cut_up <- 0.2
f_cut_down <- 0.01

z_vel <- get_zvel(turb)
z_vel <- z_vel[,1]
#LISCIOOOOOOOOO
hamming <- hamming.window(length(z_vel))
hamming <- hamming/sum(hamming)*length(z_vel)
z_vel <- z_vel*hamming
fft_zvel<- dofft(z_vel,sonic_fqc)
down_smooth<-LowPassfilter.data(fft_zvel$freq,fft_zvel$fft_vel,f_cut_up)
tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
z_vel <- Re(tot_smooth$vel)/hamming
#LISCIATO

time_stamp <- seq(from=0, to=length(z_vel)-1)*(1/sonic_fqc)
numb <- length(z_vel)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
cat("* Number of blocks: ",numb,"\n")
matrix_blocks <- matrix(ncol = dim_bl*sonic_fqc ,nrow = numb)
sigma_vis_z <- c(1:numb)
mark <- c(1:(numb-1))
mark2 <- c(1:floor(length(z_vel)/2-(dim_bl*sonic_fqc*0.5)))
for(block in 1:numb){
  sig <- signal.partition(time_stamp, z_vel, block, dim_bl)
  sigma_vis_z[block] <- sd(sig$value)
  matrix_blocks[block,] <- sig$value
}
sigma_totale_zvel <- c(sigma_totale_zvel, sigma_vis_z)

 graph_idx <- assign("graph_idx",0, envir = globalenv())
 apply(matrix_blocks, 1, function(x){
   png(paste(markov_path,"/zvel_block_markov_", graph_idx, ".png" ,sep = ''),
       width=800, height=600)
   plot(x, type='l', xlab=paste('Time[', (1/sonic_fqc), 's]'), ylab='Velocity[m/s]', main='Vertical velocity profile: time domain')
   dev.off()
   graph_idx <- assign("graph_idx",graph_idx+1, envir = globalenv())
   })
 
 
 for(riga_bl in 1:(numb-1)){
   a <- matrix_blocks[riga_bl, ]
   b <- matrix_blocks[(riga_bl+1), ]
   mark[riga_bl] <- cor(a,b)
 }
 
 for(block_mat in 1:numb){
   for(shift_pad in 1:(floor(dim_bl*sonic_fqc*0.5)-1)){
     c <- matrix_blocks[block_mat,1:((length(matrix_blocks[block_mat,])-shift_pad))]
     d <- matrix_blocks[block_mat,(shift_pad):(length(matrix_blocks[block_mat,])-1)]
     mark2[(block_mat-1)*(floor(dim_bl*sonic_fqc*0.5)-1)+shift_pad] <- cor(c,d)
   }
 }
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("Markov_",sub(".dat",'',filename[i]),"wvel.png",sep = '')))
   dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))  
   par(mfrow=c(2,1))
   plot(mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
   plot(mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
   for(graph_bl in 2:numb){
     lines(mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
   }
   par(mfrow=c(1,1))
 dev.off()
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("std_blocks_",sub(".dat",'',filename[i]),"wvel.png",sep = '')))
 plot(sigma_vis_z)
 dev.off()
 
 rm(z_vel)
 #*****************************************************
 #*************** U VELOCITY *****************
 #*****************************************************
 
 # Extracting blocks of 5 minutes from original dataset
 
 u_vel <- get_uvel(turb)
 u_vel <- u_vel[,1]
 hamming <- hamming.window(length(u_vel))
 hamming <- hamming/sum(hamming)*length(u_vel)
 u_vel <- u_vel*hamming
 fft_uvel<- dofft(u_vel,sonic_fqc)
 down_smooth<-LowPassfilter.data(fft_uvel$freq,fft_uvel$fft_vel,f_cut_up)
 tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
 u_vel <- Re(tot_smooth$vel)/hamming
 
 
 time_stamp <- seq(from=0, to=length(u_vel)-1)*(1/sonic_fqc)
 numb <- length(u_vel)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
 # seconds, not in 0.1s...
 cat("* Number of blocks: ",numb,"\n")
 matrix_blocks <- matrix(ncol = dim_bl*sonic_fqc ,nrow = numb)
 sigma_vis_y <- c(1:numb)
 mark <- c(1:(numb-1))
 mark2 <- c(1:floor(length(u_vel)/2-(dim_bl*sonic_fqc*0.5)))
 for(block in 1:numb){
   sig <- signal.partition(time_stamp, u_vel, block, dim_bl)
   sigma_vis_y[block] <- sd(sig$value)
   matrix_blocks[block,] <- sig$value
 }
 sigma_totale_yvel <- c(sigma_totale_yvel, sigma_vis_y)
 
 
 graph_idx <- assign("graph_idx",0, envir = globalenv())
 apply(matrix_blocks, 1, function(x){
   png(paste(markov_path,"/uvel_block_markov_", graph_idx, ".png" ,sep = ''),
       width=800, height=600)
   plot(x, type='l', xlab=paste('Time[', (1/sonic_fqc), 's]'), ylab='Velocity[m/s]', main='Vertical velocity profile: time domain')
   dev.off()
   graph_idx <- assign("graph_idx",graph_idx+1, envir = globalenv())
 })

 
 for(riga_bl in 1:(numb-1)){
   a <- matrix_blocks[riga_bl, ]
   b <- matrix_blocks[(riga_bl+1), ]
   mark[riga_bl] <- cor(a,b)
 }
 
 for(block_mat in 1:numb){
   for(shift_pad in 1:(floor(dim_bl*sonic_fqc*0.5)-1)){
     c <- matrix_blocks[block_mat,1:((length(matrix_blocks[block_mat,])-shift_pad))]
     d <- matrix_blocks[block_mat,(shift_pad):(length(matrix_blocks[block_mat,])-1)]
     mark2[(block_mat-1)*(floor(dim_bl*sonic_fqc*0.5)-1)+shift_pad] <- cor(c,d)
   }
 }
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("Markov_",sub(".dat",'',filename[i]),"uvel.png",sep = '')))
 dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))  
 par(mfrow=c(2,1))
 plot(mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
 plot(mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
 for(graph_bl in 2:numb){
   lines(mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
 }
 par(mfrow=c(1,1))
 dev.off()
 
 rm(u_vel)
   
 #*****************************************************
 #*************** V VELOCITY *****************
 #*****************************************************
 
 # Extracting blocks of 5 minutes from original dataset
 
 v_vel <- get_vvel(turb)
 v_vel <- v_vel[,1]
 hamming <- hamming.window(length(v_vel))
 hamming <- hamming/sum(hamming)*length(v_vel)
 v_vel <- v_vel*hamming
 fft_vvel<- dofft(v_vel,sonic_fqc)
 down_smooth<-LowPassfilter.data(fft_vvel$freq,fft_vvel$fft_vel,f_cut_up)
 tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
 v_vel <- Re(tot_smooth$vel)/hamming
 
 time_stamp <- seq(from=0, to=length(v_vel)-1)*(1/sonic_fqc)
 numb <- length(v_vel)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
 # seconds, not in 0.1s...
 cat("* Number of blocks: ",numb,"\n")
 matrix_blocks <- matrix(ncol = dim_bl*sonic_fqc ,nrow = numb)
 sigma_vis_x <- c(1:numb)
 mark <- c(1:(numb-1))
 mark2 <- c(1:floor(length(v_vel)/2-(dim_bl*sonic_fqc*0.5)))
 for(block in 1:numb){
   sig <- signal.partition(time_stamp, v_vel, block, dim_bl)
   sigma_vis_x[block] <- sd(sig$value)
   matrix_blocks[block,] <- sig$value
 }
 sigma_totale_xvel <- c(sigma_totale_xvel, sigma_vis_x)
 
 graph_idx <- assign("graph_idx",0, envir = globalenv())
 apply(matrix_blocks, 1, function(x){
   png(paste(markov_path,"/vvel_block_markov_", graph_idx, ".png" ,sep = ''),
       width=800, height=600)
   plot(x, type='l', xlab=paste('Time[', (1/sonic_fqc), 's]'), ylab='Velocity[m/s]', main='Vertical velocity profile: time domain')
   dev.off()
   graph_idx <- assign("graph_idx",graph_idx+1, envir = globalenv())
 })
 
 
 for(riga_bl in 1:(numb-1)){
   a <- matrix_blocks[riga_bl, ]
  b <- matrix_blocks[(riga_bl+1), ]
  mark[riga_bl] <- cor(a,b)
}

for(block_mat in 1:numb){
  for(shift_pad in 1:(floor(dim_bl*sonic_fqc*0.5)-1)){
    c <- matrix_blocks[block_mat,1:((length(matrix_blocks[block_mat,])-shift_pad))]
    d <- matrix_blocks[block_mat,(shift_pad):(length(matrix_blocks[block_mat,])-1)]
    mark2[(block_mat-1)*(floor(dim_bl*sonic_fqc*0.5)-1)+shift_pad] <- cor(c,d)
  }
}

png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("Markov_",sub(".dat",'',filename[i]),"vvel.png",sep = '')))
dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))
par(mfrow=c(2,1))
plot(mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
plot(mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
for(graph_bl in 2:numb){
  lines(mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
}
par(mfrow=c(1,1))
dev.off()

rm(v_vel)








