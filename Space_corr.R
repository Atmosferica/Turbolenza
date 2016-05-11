source('functions.R')
source('turbulence_class.R')

filename_tot<-c("data/SPAZIALE/LiCor/20151218.11r.dat","data/SPAZIALE/Emission/20151218.11r.dat")
corr <- NULL



data <- read.csv(filename_tot[1], skip=2, header=TRUE)
turbL <- as.turbulence(data)
z_vel_L_dop <- get_zvel(turbL)
z_vel_L_dop <- z_vel_L_dop[,1]
z_vel_L <- z_vel_L_dop[c(TRUE,FALSE)]

data <- read.csv(filename_tot[2], header=TRUE)
turbE <- as.turbulence(data)
z_vel_E <- get_zvel(turbE)
z_vel_E <- z_vel_E[,1]

dimension <- min(length(z_vel_E),length(z_vel_L))

load("mat_bloc_20160129.15r.dat")
mat_emission <- matrix_blocks 
rm(matrix_blocks)


load("data/Fontanella2/mat_bloc_20160129.15r.dat")
mat_control <- matrix_blocks 
rm(matrix_blocks)


for(block in seq_along(mat_emission[,1])){
	appo <- cor(mat_emission[block,],mat_control[block,])
	corr <- c(corr,appo)
}





#space_corr <- data.frame(LiCor=z_vel_L[1:dimension], Emission=z_vel_E[1:dimension])





  
