# ******* Orbital Method ***************

markov_path <- paste(name_dir[i], '/markov', sep='')
create_directory(markov_path)

# Finding autocorrelation for horizontal velocity

hor_vel <- get_hvel(turb)
x <- hor_vel[,1]
x.a <- x[-length(x)] # deleting last item
x.b <- x[-1] # deleting first item
rm(x)
x <- as.data.frame(cbind(x.a, x.b)) # ggplot needs a data.frame
# smoothScatter(x.a, x.b)
correl <- cor(x.a, x.b)
g1 <- ggplot(data=x, aes(x=x.a, y=x.b)) + ggtitle('Autocorrelation for horizontal velocity') +
      stat_density2d(geom="tile", aes(fill=..density..^0.25), contour=F) +
      scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
      xlab('Hor_vel without last item') + ylab('Hor_vel without first item') +
      annotate('text', x=155, y=15, label=paste('Correlation: ', round(correl, 2), sep=''), )
g1
ggsave(paste(markov_path, '/autocorrelation_hvel.png', sep=''))
rm(g1)

# The same thing for vertical velocity

z_vel <- get_zvel(turb)
z <- z_vel[,1]
z.a <- z[-length(z)] # deleting last item
z.b <- z[-1] # deleting first item
rm(z)
z <- as.data.frame(cbind(z.a, z.b)) # ggplot needs a data.frame
correl <- cor(z.a, z.b)
g1 <- ggplot(data=z, aes(x=z.a, y=z.b)) + ggtitle('Autocorrelation for vertical velocity') +
  stat_density2d(geom="tile", aes(fill=..density..^0.25), contour=F) +
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  xlab('Hor_vel without last item') + ylab('Hor_vel without first item') +
  annotate('text', x=70, y=5, label=paste('Correlation: ', round(correl, 2), sep=''), )
g1
ggsave(paste(markov_path, '/autocorrelation_zvel.png', sep=''))
rm(g1)