source('functions.R')
source('turbulence_class.R')

filename_tot<-c("data/SPAZIALE/LiCor/20151218.11r.dat","data/SPAZIALE/Emission/20151218.11r.dat")

z_vel_L<-NULL

data <- read.csv(filename_tot[1], skip=2, header=TRUE)
turbL <- as.turbulence(data)
z_vel_L_dop <- get_zvel(turbL)

for(i in seq(1,length(z_vel_L),by=2)){
  appo <- z_vel_L_dop[i]
  z_vel_L <- c(z_vel_L,appo)
}


data <- read.csv(filename_tot[2], header=TRUE)
turbE <- as.turbulence(data)
z_vel_E <- get_zvel(turbE)
z_vel_E <- z_vel_E[,1]

dimension <- min(length(z_vel_E),length(z_vel_L))

space_corr <- data.frame(LiCor=z_vel_L[1:dimension], Emission=z_vel_E[1:dimension])





  