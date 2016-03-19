library(lattice)
library(ggplot2)
library(methods)
library(e1071)

source('functions.R')
source('turbulence_class.R')

# var_code <- list.files('.', pattern='*.dat') # listing all the files in the working dir
# var_code <- sub('.dat','', var_code) # removing the '.dat' at the end of the filename
# It could be useful to separate the name in two parts: the code of the station and the hour
# (useful for creating directories and putting hour in plots)



# Extracted data from csv using the script convert_cvs.awk. 
# for(i in 1:length(var_code))
# {
# data <- read.csv('./20160129.17r.dat')
# 
# data <- read.csv(var_code[i])
#   
# Converted data (of class data.frame) into an object of class turbulence
turb <- as.turbulence(data)

turb <- set_hvel(turb) # for setting horizontal_velocity in turbulence class
velH_T <- get_hvel(turb)
velZ_T <- get_zvel(turb)
Npoint <- length(velZ_T[,1])

par(mfrow=c(2,1))
c1 = cor(velH_T[,1],velH_T[,2])
plot(velH_T[,1], velH_T[,2], type="p", pch=20, xlab = "Velocity-H[m/s]", ylab = "Temperature[C]", 
	main='Scatterplot horizontal velocity vs. temperature', sub=paste('Correlation: ', round(c1,2), sep=''))


c2 = cor(velZ_T[,1],velZ_T[,2])
plot(velZ_T[,1], velZ_T[,2], type="p", pch=20, xlab = "Velocity-Z[m/s]", ylab = "Temperature[C]",
	main='Scatterplot vertical velocity vs.  temperature', sub=paste('Correlation: ', round(c2,2), sep=''))

scatplot <- recordPlot()
print_plot(scatplot, 1200, 900, paste('./grafici_output/scatterplot_', var_code[i], sep=''))
rm(scatplot)
par(mfrow=c(1,1))

# Plotting u and temperature vs. time in the same plot
u_vel <- get_uvel(turb)
plot(u_vel[,1], type='l', cex=0.5, xlim=c(0,120), ylim=c(0,15), main='U velocity/temperature vs. time',
     sub=paste('Correlation uvel/temp: ', cor(u_vel[,1],u_vel[,2]), sep=''))
points(u_vel[,2], type='l', cex=0.5, col='green')
#abline(v=47)
#abline(v=100)
#abline(v=116)
legend(1,7,c('u','temp'), c('black','green'))
cor(u_vel[,1],u_vel[,2])

temp <- recordPlot()
print_plot(temp, 1200, 900, paste('./grafici_output/xvel_temp_', var_code[i], sep=''))

# Plotting w and temperature vs. time in the same plot
z_vel <- get_zvel(turb)
plot(z_vel[,1], type='l', cex=0.5, xlim=c(0,120), ylim=c(-1,15), main='W velocity/temperature vs. time',
     sub=paste('Correlation w_vel/temp: ', cor(z_vel[,1],z_vel[,2]), sep=''))
points(z_vel[,2], type='l', cex=0.5, col='green')
#abline(v=47)
#abline(v=100)
#abline(v=116)
legend(1,7,c('z','temp'), c('black','green'))
cor(z_vel[,1],z_vel[,2])
temp <- recordPlot()
print_plot(temp, 1200, 900, paste('./grafici_output/zvel_temp_', var_code[i], sep=''))




# }



#write(c1,stdout())
#write(c2,stdout())

