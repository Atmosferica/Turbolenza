# ******* Markov Test ***************
source('markov_functions.R')


markov_path <- paste(name_dir[i], '/markov', sep='')
create_directory(markov_path)
dim_bl <- 300
# Extracting blocks of 5 minutes from original dataset

f_cut_up <- 0.015
f_cut_down <- 0.01

z_vel <- get_zvel(turb)
z_vel <- z_vel[,1]
u_vel <- get_uvel(turb)
u_vel <- u_vel[,1]
v_vel <- get_vvel(turb)
v_vel <- v_vel[,1]
time_stamp <- seq(from=0, to=length(z_vel)-1)*(1/sonic_fqc)
numb <- length(z_vel)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...

results_list_z <- test_markov(z_vel, time_stamp, numb, dim_bl, sonic_fqc)
results_list_u <- test_markov(u_vel, time_stamp, numb, dim_bl, sonic_fqc)
results_list_v <- test_markov(v_vel, time_stamp, numb, dim_bl, sonic_fqc)



sigma_totale_zvel <- c(sigma_totale_zvel, results_list_z$sigma)
sigma_totale_yvel <- c(sigma_totale_yvel, results_list_u$sigma)
sigma_totale_xvel <- c(sigma_totale_xvel, results_list_v$sigma)


#Graficili velocita` z ######################################################################

 graph_idx <- assign("graph_idx",0, envir = globalenv())
 apply(results_list_z$matrix_blocks, 1, function(x){
   png(paste(markov_path,"/zvel_block_markov_", graph_idx, ".png" ,sep = ''),
       width=800, height=600)
   plot(x, type='l', xlab=paste('Time[', (1/sonic_fqc), 's]'), ylab='Velocity[m/s]', main='Vertical velocity profile: time domain')
   dev.off()
   graph_idx <- assign("graph_idx",graph_idx+1, envir = globalenv())
   })
 
rm(graph_idx) # removing global variable
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("Markov_",sub(".dat",'',filename[i]),"wvel.png",sep = '')))
   dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))  
   par(mfrow=c(2,1))
   plot(results_list_z$mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
   plot(results_list_z$mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
   for(graph_bl in 2:numb){
     lines(results_list_z$mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
   }
   par(mfrow=c(1,1))
 dev.off()
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("std_blocks_",sub(".dat",'',filename[i]),"wvel.png",sep = '')))
 plot(results_list_z$sigma)
 dev.off()
 
 rm(z_vel)
 
 
 #*****************************************************
 #*************** GRAFICI VELOCITA` U *****************
 #*****************************************************
 graph_idx <- assign("graph_idx",0, envir = globalenv())
 apply(results_list_u$matrix_blocks, 1, function(x){
   png(paste(markov_path,"/uvel_block_markov_", graph_idx, ".png" ,sep = ''),
       width=800, height=600)
   plot(x, type='l', xlab=paste('Time[', (1/sonic_fqc), 's]'), ylab='Velocity[m/s]', main='Vertical velocity profile: time domain')
   dev.off()
   graph_idx <- assign("graph_idx",graph_idx+1, envir = globalenv())
 })
 
 rm(graph_idx) # removing global variable
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("Markov_",sub(".dat",'',filename[i]),"uvel.png",sep = '')))
 dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))  
 par(mfrow=c(2,1))
 plot(results_list_u$mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
 plot(results_list_u$mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
 for(graph_bl in 2:numb){
   lines(results_list_u$mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
 }
 par(mfrow=c(1,1))
 dev.off()
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("std_blocks_",sub(".dat",'',filename[i]),"uvel.png",sep = '')))
 plot(results_list_u$sigma)
 dev.off()
 
 rm(u_vel)
 
   
 #*****************************************************
 #*************** V VELOCITY *****************
 #*****************************************************

 graph_idx <- assign("graph_idx",0, envir = globalenv())
 apply(results_list_v$matrix_blocks, 1, function(x){
   png(paste(markov_path,"/vvel_block_markov_", graph_idx, ".png" ,sep = ''),
       width=800, height=600)
   plot(x, type='l', xlab=paste('Time[', (1/sonic_fqc), 's]'), ylab='Velocity[m/s]', main='Vertical velocity profile: time domain')
   dev.off()
   graph_idx <- assign("graph_idx",graph_idx+1, envir = globalenv())
 })
 
 rm(graph_idx) # removing global variable
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("Markov_",sub(".dat",'',filename[i]),"vvel.png",sep = '')))
 dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))  
 par(mfrow=c(2,1))
 plot(results_list_v$mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
 plot(results_list_v$mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
 for(graph_bl in 2:numb){
   lines(results_list_v$mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
 }
 par(mfrow=c(1,1))
 dev.off()
 
 png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("std_blocks_",sub(".dat",'',filename[i]),"vvel.png",sep = '')))
 plot(results_list_v$sigma)
 dev.off()
 
 rm(v_vel)