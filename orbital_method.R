# ******* Orbital Method ***************

hor_vel <- get_hvel(turb)
x <- hor_vel[,1]
x.a <- x[1:6881]
x.b <- x[21:6901]
par(mfrow=c(1,1))
smoothScatter(x.a, x.b)
cor(x.a, x.b)

x11()
z_vel <- get_zvel(turb)
z <- z_vel[,1]
z.a <- z[1:6881]
z.b <- z[21:6901]
par(mfrow=c(1,1))
smoothScatter(z.a, z.b)
cor(z.a, z.b)