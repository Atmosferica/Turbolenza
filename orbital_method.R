# ******* Orbital Method ***************

markov_path <- paste(name_dir[i], '/markov', sep='')
create_directory(markov_path)
dim_bl <- 300
# Extracting blocks of 5 minutes from original dataset

z_vel <- get_zvel(turb)
z_vel <- z_vel[,1]
time_stamp <- seq(from=0, to=length(z_vel)-1)*(1/sonic_fqc)
numb <- length(z_vel)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
for(block in 1:numb){
  sig <- signal.partition(time_stamp, z_vel, block, dim_bl)

  # computing the autocorrelation (we need it for checking the markovianity)
  mark <- autocorr(z_vel[((dim_bl*sonic_fqc)*(block-1)+1):(dim_bl*sonic_fqc*block)], sig)
  # plotting the exponential of lm vs. data constraining the intercept to 0
  mark2 <- log(mark[1:20])
  exp_fit <- lm(mark2 ~ c(1:length(mark2))+0)
  model <- exp(predict(exp_fit))
  png(filename=paste(markov_path, '/exponential_fit_zvel_', block,'.png', sep=''), height=400, width=600)
  plot(exp(mark2), type='p', pch=19, cex=0.5, ylim=c(0,1), main='Exponential
       fit: z velocity', xlab=paste('Time [', 1/sonic_fqc, 's]', sep=''),
       ylab='Autocorrelation', sub=paste('Coeff: ', round(summary(exp_fit)$coefficients[1,1],2),
                                         ' R^2: ', round(summary(exp_fit)$r.squared,2),
                                         ' Decorrelation time:', abs(round((1/summary(exp_fit)$coefficients[1,1]), 2))*(1/sonic_fqc), 's', sep=''))
  points(model, type='l', col='red')
  dev.off()
}

# ******************************************************
# ************* HORIZONTAL VELOCITY ********************
# ******************************************************


# Extracting blocks of 5 minutes from original dataset

turb <- set_hvel(turb)
h_vel <- get_hvel(turb)
h_vel <- h_vel[,1]
time_stamp <- seq(from=0, to=length(h_vel)-1)*(1/sonic_fqc)
numb <- length(h_vel)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
for(block in 1:numb){
  sig <- signal.partition(time_stamp, h_vel, block, dim_bl)

  # computing the autocorrelation (we need it for checking the markovianity)
  mark <- autocorr(h_vel[((dim_bl*sonic_fqc)*(block-1)+1):(dim_bl*sonic_fqc*block)], sig)
  # plotting the exponential of lm vs. data constraining the intercept to 0
  mark2 <- log(mark[1:20])
  exp_fit <- lm(mark2 ~ c(1:length(mark2))+0)
  model <- exp(predict(exp_fit))
  png(filename=paste(markov_path, '/exponential_fit_hvel_', block, '.png', sep=''), height=400, width=600)
  plot(exp(mark2), type='p', pch=19, cex=0.5, ylim=c(0,1), main='Exponential
       fit: h velocity', xlab=paste('Time [', 1/sonic_fqc, 's]', sep=''),
       ylab='Autocorrelation', sub=paste('Coeff: ', round(summary(exp_fit)$coefficients[1,1],2),
                                         ' R^2: ', round(summary(exp_fit)$r.squared,2),
                                         ' Decorrelation time:', abs(round((1/summary(exp_fit)$coefficients[1,1]), 2))*(1/sonic_fqc),
                                         's', sep=''))
  points(model, type='l', col='red')
  dev.off()
} 

