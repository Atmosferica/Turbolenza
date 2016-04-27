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

  
  
  
  
  


# ******************************************************
# ************* HORIZONTAL VELOCITY ********************
# ******************************************************


# Extracting blocks of 5 minutes from original dataset

#h_vel <- get_hvel(turb)
#h_vel <- h_vel[,1]
#time_stamp <- seq(from=0, to=length(h_vel)-1)*0.1
#numb <- length(h_vel)%/%3000 # number of blocks: watch out, blocks are in
## seconds, not in 0.1s...
#for(block in 1:numb){
#  sig <- signal.partition(time_stamp, h_vel, block, 300)


#  # computing the autocorrelation (we need it for checking the markovianity)
#  mark <- c(1:length(sig[,2]))
#  for(j in 1:length(sig[,2])){
#    h_a <- h_vel[1:(length(h_vel)-j)]
#    h_b <- h_vel[(j+1):length(h_vel)]
#    mark[j] <-   cor(h_a, h_b)
#  }


#  # for the exponential fit, fitting the log of the data with lm
#  # plotting the exponential of lm vs. data
#  mark2 <- mark[1:30]
#  exp_fit <- lm(log(mark2) ~ c(1:length(mark2)))
#  model <- exp(predict(exp_fit))
#  plot(mark2, type='p', pch=19, cex=0.5)
#  points(model, type='l')


#  mark2 <- log(mark[1:20])
#  exp_fit <- lm(mark2 ~ c(1:length(mark2))+0)
#  model <- exp(predict(exp_fit))
#  plot(exp(mark2), type='p', pch=19, cex=0.5)
#  points(model, type='l')

  # plotting the log of the data vs. lm linear model
#  mark2 <- mark[1:30]
#  exp_fit <- lm(log(mark2) ~ c(1:length(mark2)))
#  model <- predict(exp_fit)
##  plot(log(mark2), type='p', pch=19, cex=0.5)
#  points(model, type='l')

  # fitting with a 5-th degree polynomial
#  par(mfrow=c(2,2))
#  mark2 <- mark[1:30]
#  predic <- c(1:length(mark2))
#  pol_fit <- lm(mark2 ~ poly(predic, 5))
#  model <- predict(pol_fit)
#  plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 30", xlab="Time [10^(-1)s]",
#       ylab="Correlation")
#  points(model, type='l', col='red', lwd=1.3)

#  mark2 <- mark[1:100]
#  predic <- c(1:length(mark2))
#  pol_fit <- lm(mark2 ~ poly(predic, 5))
#  model <- predict(pol_fit)
#   plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 100", xlab="Time [10^(-1)s]",
#       ylab="Correlation")
#  points(model, type='l', col='red', lwd=1.3)-

#  mark2 <- mark[1:300]
#  predic <- c(1:length(mark2))
#  pol_fit <- lm(mark2 ~ poly(predic, 5))
#  model <- predict(pol_fit)
#  plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 300", xlab="Time [10^(-1)s]",
#       ylab="Correlation")
#  points(model, type='l', col='red', lwd=1.3)

#  mark2 <- mark
#  predic <- c(1:length(mark2))
#  pol_fit <- lm(mark2 ~ poly(predic, 5))
#  model <- predict(pol_fit)
#  plot(mark2, type='p', pch=19, cex=0.2, main=paste("5th degree, npoints = ", length(mark2), sep=''), xlab="Time [10^(-1)s]",
#       ylab="Correlation")
#  points(model, type='l', col='red', lwd=1.3)

#  par(mfrow=c(1,1))

#}