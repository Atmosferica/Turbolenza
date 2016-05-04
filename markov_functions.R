source('functions.R')

test_markov<-function(vel, time_stamp, numb, dim_bl, sonic_fqc){
  cat("* Number of blocks: ",numb,"\n")
  matrix_blocks <- matrix(ncol = dim_bl*sonic_fqc ,nrow = numb)
  sigma_block <- c(1:numb)
  mark <- c(1:(numb-1))
  mark2 <- c(1:floor(length(vel)/2-(dim_bl*sonic_fqc*0.5)))
  for(block in 1:numb){
    sig <- signal.partition(time_stamp, vel, block, dim_bl)
    sigma_block[block] <- sd(sig$value)
    matrix_blocks[block,] <- sig$value
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
  result<-list(sigma=sigma_block, mark, mark2)
  
  return(result)
}