#Here there is the programme that studies the skewness and kurtosis coefficient of our data

path_output <- paste('grafici_output/', line, '/', dati[1], '/', sep = '')

sk_plot(x_sk, path_output ,"x")
sk_plot(y_sk, path_output, "y")
sk_plot(z_sk, path_output, "z")