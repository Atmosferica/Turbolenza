test_markov<-function(vel, time_stamp, numb, dim_bl, sonic_fqc){
	hamming <- hamming.window(length(z_vel))
	hamming <- hamming/sum(hamming)*length(z_vel)
	z_vel <- z_vel*hamming
	fft_zvel<- dofft(z_vel,sonic_fqc)
	down_smooth<-LowPassfilter.data(fft_zvel$freq,fft_zvel$fft_vel,f_cut_up)
	tot_smooth<-HiPassfilter.data(down_smooth$freq,down_smooth$fft_vel,f_cut_down)
	z_vel <- Re(tot_smooth$vel)/hamming 
	
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
	result<-list(sigma=sigma_block, mark=mark, mark2=mark2, matrix_blocks=matrix_blocks)
	return(result)
}

# Function for the exponential fit of autocorrelation
expon_fit <- function(result_list, dim_shift_mezzi, n_block){
  mark2x <- c(1:(dim_shift_mezzi))
  mark2y <- result_list$mark2[(dim_shift_mezzi*(n_block -1)+1):(n_block*(dim_shift_mezzi))]
  #cat(paste('mark2x: ', length(mark2x), '\n', sep=''))
  #cat(paste('mark2y: ', length(mark2y), '\n', sep=''))
  df <- data.frame(mark2x, mark2y)
  model_exp <- nls(mark2y ~ I(a * exp(-(b * mark2x))), data=df, 
                   start=list(a=1, b=0.05), trace = T)
  pred <- seq(from=0.1, to=300, length.out=length(mark2x))
  #cat(paste('pred: ', length(pred), '\n', sep=''))
  predictions <- predict(model_exp, newdata=data.frame(pred=pred))
  #cat(paste('predictions: ', length(predictions), '\n', sep=''))
  pars <- model_exp$m$getPars()[2]
  # In the case of last block the length of predictions is usually shorter as
  # the length of mark2x
  if(length(predictions)!=length(mark2x)){
    mark2x <- mark2x[1:length(predictions)]
    mark2y <- mark2x[1:length(predictions)]
    df <- data.frame(mark2x, mark2y)
  }
  to_return <- list(df=df, predictions=predictions, pars=pars)
}
