# # Little trial with ggplot, from here:
# 
# turb_temporary <- as.data.frame(turb) # ggplot2 needs a dataframe
# 
# g1 <- ggplot(data=turb_temporary, aes(x=c(1:length(u)), y=u)) + geom_line(aes(colour='u')) + 
#       xlim(0,120) + ylim(-1,2.5) + theme(legend.position="left")
# g1 <- g1 + geom_line(aes(x=c(1:length(w)), y=w, colour='w')) 
# g1 <- g1 + geom_line(aes(x=c(1:length(temp)), y=temp, colour='temp')) + 
#       scale_colour_manual(name='', values=c('u'='blue', 'w'='green', 'temp'='red')) +
#       xlab('Time(s)') + ylab('Velocity(m/s)') + ggtitle('U and W Velocity-Temperature vs Time') +
#       theme(plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)))
# g2 <- ggplot(data=turb_temporary, aes(x=c(1:length(temp)), y=temp)) + geom_line(aes(colour='temp')) + xlim(0,120) + 
#       ylim(12,13.5)  + theme_bw() + labs(x="Date", y=expression(paste("Temperature ( ", degree ~ C, " )"))) +
#       theme(panel.background = element_rect(fill = NA)) 
# g <- dual_axis(g1, g2)
# grid.draw(g)
# 
# # to here
