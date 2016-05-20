direction <- get_direction(turb)
hist_temp <- hist(direction, breaks=nclass.FD(direction))
hist_temp_cont <- hist_temp$counts
hist_temp_mids <- hist_temp$mids
df <- data.frame(counts=hist_temp_cont, pos=hist_temp_mids)

#hour <- substr(x = var_code, .........*)

g1 <- ggplot(df, aes(factor(pos), counts, fill = factor(pos))) + geom_bar(stat='identity')
g1 <- g1 + coord_polar() + labs(x = "", y = "") + scale_y_continuous() + 
  theme(legend.position='none') 
g1 <- g1 + geom_bar(aes(width = 1), position = "stack",
                    stat = "identity", fill = NA, colour = "white")
g1 <- g1 + labs(title='Wind Direction')

g1
# Must add a place to save polar chart and hour in the title...
