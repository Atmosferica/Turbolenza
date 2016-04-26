# ******* Orbital Method ***************

markov_path <- paste(name_dir[i], '/markov', sep='')
create_directory(markov_path)
dim_bl <- 5
# Extracting blocks of 5 minutes from original dataset

z_vel <- get_zvel(turb)
z_vel <- z_vel[,1]
time_stamp <- seq(from=0, to=length(z_vel)-1)*0.1
numb <- length(z_vel)%/%(dim_bl*10) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
for(block in 1:numb){
  sig <- signal.partition(time_stamp, z_vel, block, dim_bl)
}

# computing the autocorrelation (we need it for checking the markovianity)
mark <- c(1:length(sig[,2]))
for(j in 1:length(sig[,2])){
  z_a <- z_vel[1:(length(z_vel)-j)]
  z_b <- z_vel[(j+1):length(z_vel)]
  mark[j] <-   cor(z_a, z_b)
}


# for the exponential fit, fitting the log of the data with lm
# plotting the exponential of lm vs. data
mark2 <- mark[1:20]
exp_fit <- lm(log(mark2) ~ c(1:length(mark2)))
model <- exp(predict(exp_fit))
plot(mark2, type='p', pch=19, cex=0.5)
points(model, type='l')

mark2 <- log(mark[1:20])
exp_fit <- lm(mark2 ~ c(1:length(mark2))+0)
model <- exp(predict(exp_fit))
plot(exp(mark2), type='p', pch=19, cex=0.5, ylim=c(0,1))
points(model, type='l')


# plotting the log of the data vs. lm linear model
mark2 <- mark[1:30]
exp_fit <- lm(log(mark2) ~ c(1:length(mark2)))
model <- predict(exp_fit)
plot(log(mark2), type='p', pch=19, cex=0.5)
points(model, type='l')

# fitting with a 5-th degree polynomial
par(mfrow=c(2,2))
mark2 <- mark[1:30]
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 30", xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

mark2 <- mark[1:100]
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 100", xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

mark2 <- mark[1:300]
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 300", xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

mark2 <- mark
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main=paste("5th degree, npoints = ", length(mark2), sep=''), xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

par(mfrow=c(1,1))

# ******************************************************
# ************* HORIZONTAL VELOCITY ********************
# ******************************************************


# Extracting blocks of 5 minutes from original dataset

h_vel <- get_hvel(turb)
h_vel <- h_vel[,1]
time_stamp <- seq(from=0, to=length(h_vel)-1)*0.1
numb <- length(h_vel)%/%3000 # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
for(block in 1:numb){
  sig <- signal.partition(time_stamp, h_vel, block, 300)
}

# computing the autocorrelation (we need it for checking the markovianity)
mark <- c(1:length(sig[,2]))
for(j in 1:length(sig[,2])){
  h_a <- h_vel[1:(length(h_vel)-j)]
  h_b <- h_vel[(j+1):length(h_vel)]
  mark[j] <-   cor(h_a, h_b)
}


# for the exponential fit, fitting the log of the data with lm
# plotting the exponential of lm vs. data
mark2 <- mark[1:30]
exp_fit <- lm(log(mark2) ~ c(1:length(mark2)))
model <- exp(predict(exp_fit))
plot(mark2, type='p', pch=19, cex=0.5)
points(model, type='l')


mark2 <- log(mark[1:20])
exp_fit <- lm(mark2 ~ c(1:length(mark2))+0)
model <- exp(predict(exp_fit))
plot(exp(mark2), type='p', pch=19, cex=0.5)
points(model, type='l')

# plotting the log of the data vs. lm linear model
mark2 <- mark[1:30]
exp_fit <- lm(log(mark2) ~ c(1:length(mark2)))
model <- predict(exp_fit)
plot(log(mark2), type='p', pch=19, cex=0.5)
points(model, type='l')

# fitting with a 5-th degree polynomial
par(mfrow=c(2,2))
mark2 <- mark[1:30]
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 30", xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

mark2 <- mark[1:100]
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 100", xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

mark2 <- mark[1:300]
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main="5th degree, npoints = 300", xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

mark2 <- mark
predic <- c(1:length(mark2))
pol_fit <- lm(mark2 ~ poly(predic, 5))
model <- predict(pol_fit)
plot(mark2, type='p', pch=19, cex=0.2, main=paste("5th degree, npoints = ", length(mark2), sep=''), xlab="Time [10^(-1)s]",
     ylab="Correlation")
points(model, type='l', col='red', lwd=1.3)

par(mfrow=c(1,1))




# Finding autocorrelation for horizontal velocity
# 
# hor_vel <- get_hvel(turb)
# x <- hor_vel[,1]
# x.a <- x[-length(x)] # deleting last item
# x.b <- x[-1] # deleting first item
# rm(x)
# x <- as.data.frame(cbind(x.a, x.b)) # ggplot needs a data.frame
# # smoothScatter(x.a, x.b)
# correl <- cor(x.a, x.b)
# g1 <- ggplot(data=x, aes(x=x.a, y=x.b)) + ggtitle('Autocorrelation for horizontal velocity') +
#       stat_density2d(geom="tile", aes(fill=..density..^0.25), contour=F) +
#       scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
#       xlab('Hor_vel without last item') + ylab('Hor_vel without first item') +
#       annotate('text', x=155, y=15, label=paste('Correlation: ', round(correl, 2), sep=''), )
# #g1
# ggsave(paste(markov_path, '/autocorrelation_hvel.png', sep=''))
# rm(g1)

# The same thing for vertical velocity

# z_vel <- get_zvel(turb)
# z <- z_vel[,1]
# z.a <- z[-length(z)] # deleting last item
# z.b <- z[-1] # deleting first item
# # rm(z)
# z <- as.data.frame(cbind(z.a, z.b)) # ggplot needs a data.frame
# correl <- cor(z.a, z.b)
# g1 <- ggplot(data=z, aes(x=z.a, y=z.b)) + ggtitle('Autocorrelation for vertical velocity') +
#   stat_density2d(geom="tile", aes(fill=..density..^0.25), contour=F) +
#   scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
#   xlab('Hor_vel without last item') + ylab('Hor_vel without first item') +
#   annotate('text', x=70, y=5, label=paste('Correlation: ', round(correl, 2), sep=''), )
# #g1
# ggsave(paste(markov_path, '/autocorrelation_zvel.png', sep=''))
# rm(g1)
