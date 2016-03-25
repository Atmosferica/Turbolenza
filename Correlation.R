###### Disclaimer: if you want to run this script alone, comment
# the unused scripts in main.R instead of importing here the dataset
directory_dataset <- name_dir[i] 
velH_T <- get_hvel(turb)
velZ_T <- get_zvel(turb)
Npoint <- length(velZ_T[,1])

#***************************************************************
#Draw correlation graph
#***************************************************************

par(mfrow=c(2,1))
c1 = cor(velH_T[,1],velH_T[,2])
plot(velH_T[,1], velH_T[,2], type="p", pch=20, xlab = "Velocity-H[m/s]", ylab = "Temperature[C]", 
	main='Scatterplot horizontal velocity vs. temperature', sub=paste('Correlation: ', round(c1,2), sep=''))

# #temp_dataframe <- data.frame(hor_vel=velH_T[,1], z_vel=velZ_T[,1], temp=velH_T[,2])
# 
# g1 <- ggplot(data=temp_dataframe, aes(c(1:length(temp)), temp)) + geom_line(aes(colour=temp))
# g2 <- ggplot(data=temp_dataframe, aes(c(1:length(z_vel)), z_vel)) + geom_line(aes(colour=z_vel))
# 
# g <- dual_axis(g1, g2)



c2 = cor(velZ_T[,1],velZ_T[,2])
plot(velZ_T[,1], velZ_T[,2], type="p", pch=20, xlab = "Velocity-Z[m/s]", ylab = "Temperature[C]",
	main='Scatterplot vertical velocity vs.  temperature', sub=paste('Correlation: ', round(c2,2), sep=''))

#***************************************************************
#Save correlation graph on file
#***************************************************************

scatplot <- recordPlot()
name <- paste(directory_dataset,"/corr_graph_",var_code[i],".png",sep = '')
print_plot(scatplot, 1200, 900, name)
rm(scatplot)
par(mfrow=c(1,1))

#***************************************************************
# Plotting u  w and temperature vs. time in the same plot
# A naive approach to double Y axes
#***************************************************************
xmin <- 0;
xmax <- 120;

u_vel <- get_uvel(turb)
z_vel <- get_zvel(turb)

t_ord <- sort(u_vel[,2][xmin:xmax])
u_ord <- sort(u_vel[,1][xmin:xmax])
z_ord <- sort(z_vel[,1][xmin:xmax])
vel_ord <- sort(cbind(u_ord,z_ord))


par(mar=c(5.1, 4.1, 4.1, 3.6))

plot(u_vel[,1], type='l', cex=0.5, 
     #xlim=c(0,120), ylim=c(-0.2,2), 
     xlim=c(xmin,xmax), ylim=c(vel_ord[1],vel_ord[length(vel_ord)]),
     main='U and W Velocity-Temperature vs Time',
     axes = FALSE,
     xlab = "Time",
     ylab = "Velocity [m/s]"
     )
axis(2, ylim=c(0,1),col="black",las=1)
axis(1, xlim=c(xmin,xmax),col="black",las=1)
par(new=TRUE)
plot(u_vel[,2], type='l', cex=0.5,
     #xlim=c(0,120), ylim=c(12,13.5),
     xlim=c(xmin,xmax), ylim=c(t_ord[1]-vel_ord[length(vel_ord)],t_ord[length(t_ord)]),
     col='green',axes = FALSE, 
     bty = "n", xlab = "", ylab = ""
     )
axis(4, ylim=c(0,1),col="green",las=1)
par(new=TRUE)
plot(z_vel[,1], type='l', cex=0.5, 
     xlim=c(xmin,xmax), ylim=c(vel_ord[1],vel_ord[length(vel_ord)]),
     col="red",axes = FALSE,
     ylab = "",xlab = ""
     )
mtext(paste("Correlation Zvel/Temp = ",round(cor(z_vel[,1],z_vel[,2]),3)),at=15, line=3)
mtext(paste("Correlation Uvel/Temp = ",round(cor(u_vel[,1],u_vel[,2]),3)),at=95 , line=3)
mtext("Temp[C]", side=4, line=2)
legend("topleft",legend=c('U-Vel',"W-Vel","Temp"),
       text.col=c("black","red","green"),pch=c("-","-","-"),
       col=c("black","red","green"))

par(mar=c(5.1, 4.1, 4.1, 1.1))

#***************************************************************
#Save Uvel/Temp graph on file
#***************************************************************

temp <- recordPlot()
name <- paste(directory_dataset,"/Vel-Temp_",var_code[i],".png",sep = '')
print_plot(temp, 1200, 900, name)

#***************************************************************
#Plotting w and temperature vs. time in the same plot
#***************************************************************

#z_vel <- get_zvel(turb)
#plot(z_vel[,1], type='l', cex=0.5, xlim=c(0,120), ylim=c(-1,15), main='W velocity/temperature vs. time',
#     sub=paste('Correlation w_vel/temp: ', cor(z_vel[,1],z_vel[,2]), sep=''))
#points(z_vel[,2], type='l', cex=0.5, col='green')
#legend(1,7,c('z','temp'), c('black','green'))
#cor(z_vel[,1],z_vel[,2])


#***************************************************************
#Save Wvel/temp graph on file
#***************************************************************

#temp <- recordPlot()
#name <- paste(directory_dataset,"/WT_",var_code[i],".png",sep = '')
#print_plot(temp, 1200, 900, name)



