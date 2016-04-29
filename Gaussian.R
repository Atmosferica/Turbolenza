#Here there is the programme that studies the skewness and kurtosis coefficient of our data

dim_bl <- 300
# Extracting blocks of 5 minutes from original dataset



time_stamp <- seq(from=0, to=length(z)-1)*(1/sonic_fqc)
numb <- length(z)%/%(dim_bl*sonic_fqc) # number of blocks: watch out, blocks are in
# seconds, not in 0.1s...
cat("* Number of blocks: ",numb,"\n")

m.x_sk <- matrix(ncol = 4 ,nrow = numb)
m.y_sk <- matrix(ncol = 4 ,nrow = numb)
m.z_sk <- matrix(ncol = 4 ,nrow = numb)

tempo<-dati

for(block in 1:numb){
  
  tempo[2]<-dati[2] + (block-1)*0.05
  m.x_sk[block,]<- boh(time_stamp, x, block, dim_bl,tempo, numb)
  m.y_sk[block,]<- boh(time_stamp, y, block, dim_bl,tempo, numb)
  m.z_sk[block,]<- boh(time_stamp, z, block, dim_bl,tempo, numb)
}


path_output <- paste('grafici_output/', line, '/',dati[1], '/', sep = '')
create_directory(path_output)
path_output <- paste('grafici_output/', line, '/',dati[1], '/', dati[2], '/', sep = '')
create_directory(path_output)

sk_plot(m.x_sk, path_output,"x")
sk_plot(m.y_sk, path_output,"y")
sk_plot(m.z_sk, path_output,"z")

