# ******* Markov Test ***************

markov_path <- paste(name_dir[i], '/markov', sep='')
create_directory(markov_path)
dim_bl <- 30
# Extracting blocks of 5 minutes from original dataset

z_vel <- get_zvel(turb)
z_vel <- z_vel[,1]
time_stamp <- seq(from=0, to=length(z_vel)-1)*0.1
numb <- length(z_vel)%/%(dim_bl*10) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
cat(numb)
matrix_blocks <- matrix(ncol = dim_bl*10 ,nrow = numb)
mark <- c(1:(numb-1))
mark2 <- c(1:floor(length(z_vel)/2-(dim_bl*10*0.5)))
for(block in 1:numb){
  sig <- signal.partition(time_stamp, z_vel, block, dim_bl)
  matrix_blocks[block,] <- with(sig,value)
}

for(k in 1:(numb-1)){
  a <- matrix_blocks[k, ]
  b <- matrix_blocks[k+1, ]
  mark[k] <- cor(a,b)
}

for(bb in 1:numb){
  for(i in 1:(floor(dim_bl*10*0.5))){
    #c <- matrix_blocks[bb,-((dim_bl*10)-i+1)]
    #d <- matrix_blocks[bb,-1]
    c <- matrix_blocks[bb,1:((dim_bl*10)-i)]
    d <- matrix_blocks[bb,(i+1):(dim_bl*10)]
    mark2[(bb-1)*(dim_bl*10*0.5)+i] <- cor(c,d)
  }
}
par(mfrow=c(2,1))
plot(mark, t='l')
plot(mark2,t='l')
par(mfrow=c(1,1))

