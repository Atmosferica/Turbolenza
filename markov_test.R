# ******* Markov Test ***************

markov_path <- paste(name_dir[i], '/markov', sep='')
create_directory(markov_path)
dim_bl <- 300
# Extracting blocks of 5 minutes from original dataset

z_vel <- get_zvel(turb)
z_vel <- z_vel[,1]
time_stamp <- seq(from=0, to=length(z_vel)-1)*(1/sonic_fqc)
numb <- length(z_vel)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
cat("* Number of blocks: ",numb,"\n")
matrix_blocks <- matrix(ncol = dim_bl*sonic_fqc ,nrow = numb)
mark <- c(1:(numb-1))
mark2 <- c(1:floor(length(z_vel)/2-(dim_bl*sonic_fqc*0.5)))
for(block in 1:numb){
  sig <- signal.partition(time_stamp, z_vel, block, dim_bl)
  #mm <- lm( sig$value ~ c(1:length(sig$value)))
  #sig$value <- sig$value - predict(mm)
  matrix_blocks[block,] <- sig$value
}

graph_idx <- assign("graph_idx",0, envir = globalenv())
apply(matrix_blocks, 1, function(x){
  png(paste(markov_path,"/prova_markov2_", graph_idx, ".png" ,sep = ''))
  plot(x, type='l', xlab='Time[0.1s]', ylab='Velocity[m/s]', main='Vertical velocity profile: time domain')
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

png(paste(paste("grafici_output",sub('data','',path_dir),sep = ""),paste("Markov_",sub(".dat",'',filename[i]),".png",sep = '')))
  dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))  
  par(mfrow=c(2,1))
  plot(mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
  plot(mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
  for(graph_bl in 2:numb){
    lines(mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
  }
  par(mfrow=c(1,1))
dev.off()

  
  
  
  
  
  
  
  
  
  
  
  
  
