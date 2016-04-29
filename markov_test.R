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
  matrix_blocks[block,] <- with(sig,value)
}

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


mark3 <- as.data.frame(mark2)
# g1 <- qplot(mark2) # qplot works, now I must understand how to make ggplot work...
# g1 <- ggplot(as.data.frame(mark2))
g1 <- ggplot(mark3, aes(x = c(seq(0.1, 300, by=1/sonic_fqc)), y = mark2[1:(dim_shift_mezzi-1)])) +
  geom_line() + labs(x='Time[s]', y='Correlation')
g1

# png(paste(markov_path,"/Markov_test.png",sep = ''))
#   dim_shift_mezzi <- (floor(dim_bl*sonic_fqc*0.5))  
#   par(mfrow=c(2,1))
#   plot(mark, t='l',xlab = "Block Index [N]", ylab = "Correlation", main="Correlation index between block N and N+1 ")
#   plot(mark2[1:(dim_shift_mezzi-1)],t='l',xlab = "Shift" , ylab = "Correlation", main="Decorrelation pattern in single block",col='black')
#   for(graph_bl in 2:numb){
#     lines(mark2[((dim_shift_mezzi)*(graph_bl-1)):((dim_shift_mezzi-1)*(graph_bl))],col=c(120+(10*graph_bl),120,120))
#   }
#   par(mfrow=c(1,1))
# dev.off()

  
  
  
  
  
  
  
  
  
  
  
  
  
  
