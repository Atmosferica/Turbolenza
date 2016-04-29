# Here I try to define a S4 class for data of ultrasonic anemometer

# This is the definition of class turbulence
turbulence <- setClass('turbulence', slots=c(u='numeric', v='numeric', w='numeric', t='numeric', h_vel='numeric', dir='numeric'))

# Method for filling the slot corresponding to horizontal velocity from class turbulence
setGeneric('set_hvel', function(object){
  standardGeneric('set_hvel')
})

setMethod('set_hvel', signature='turbulence',
          function(object){
            x_vel <- object@u                # wind x velocity (west to east)
            y_vel <- object@v                # wind y velocity (south to north)
            h_vel <- c(1:length(x_vel))      
            h_vel <- sqrt(x_vel^2 + y_vel^2) # horizontal velocity
            object@h_vel <- h_vel
            return(object)
          })

#*****************************************************************************
# Method for setting the slot corresponding to direction of wind
#*****************************************************************************

setGeneric('set_direction', function(object){
  standardGeneric('set_direction')
})

setMethod('set_direction', signature='turbulence',
	function(object){
	  x_vel <- object@u
	  y_vel <- object@v
	  dir <- c(1:length(x_vel))
	  dir <- atan2(x_vel, y_vel) # direction of wind from the North: I'm not sure, MUST CHECK!!!
	  object@dir <- dir*(180/pi)
	  return(object)
})


#******************************************************************************
# Method for extracting wind direction slot
#******************************************************************************

setGeneric('get_direction', function(object){    # To define an S4 method, I must create a generic first
  standardGeneric('get_direction')               # standardGeneric is the S4 equivalent of UseMethod
})

setMethod('get_direction', signature='turbulence', 
          function(object){
            dir <- object@dir
            return(dir)
          }
          , sealed=FALSE)

#******************************************************************************
# Method for extracting the slot corresponding 
# to horizontal velocity from class turbulence
#******************************************************************************

setGeneric('get_hvel', function(object){     # To define an S4 method, I must create a generic first
  standardGeneric('get_hvel')                # standardGeneric is the S4 equivalent of UseMethod
})

setMethod('get_hvel', signature='turbulence', 
          function(object){
            h_vel <- object@h_vel
            temperature <- object@t
	    h_vel <- cbind(h_vel,temperature)
            # Checking if slot @h_vel has already been filled with set_hvel
            if(length(object@h_vel)==0) stop('Slot empty! Did you set the value with set_hvel()?')
            return(h_vel)
          }
          , sealed=FALSE)

#******************************************************************************
# Method for extracting vertical velocity slot
#******************************************************************************

setGeneric('get_zvel', function(object){    # To define an S4 method, I must create a generic first
  standardGeneric('get_zvel')               # standardGeneric is the S4 equivalent of UseMethod
})

setMethod('get_zvel', signature='turbulence', 
          function(object){
            z_vel <- object@w
            temperature <-object@t
            z_vel <- cbind(z_vel,temperature)
            return(z_vel)
          }
          , sealed=FALSE)

#******************************************************************************
# Method for extracting u velocity slot
#******************************************************************************

setGeneric('get_uvel', function(object){    # To define an S4 method, I must create a generic first
  standardGeneric('get_uvel')               # standardGeneric is the S4 equivalent of UseMethod
})

setMethod('get_uvel', signature='turbulence', 
          function(object){
            u_vel <- object@u
            temperature <-object@t
            u_vel <- cbind(u_vel,temperature)
            return(u_vel)
          }
          , sealed=FALSE)

#******************************************************************************
# Method for extracting v velocity slot
#******************************************************************************

setGeneric('get_vvel', function(object){    # To define an S4 method, I must create a generic first
  standardGeneric('get_vvel')               # standardGeneric is the S4 equivalent of UseMethod
})

setMethod('get_vvel', signature='turbulence', 
          function(object){
            v_vel <- object@v
            temperature <-object@t
            v_vel <- cbind(v_vel,temperature)
            return(v_vel)
          }
          , sealed=FALSE)

#******************************************************************************
# Method for extracting v velocity slot
#******************************************************************************

setGeneric('get_vvel', function(object){    # To define an S4 method, I must create a generic first
  standardGeneric('get_vvel')               # standardGeneric is the S4 equivalent of UseMethod
})

setMethod('get_vvel', signature='turbulence', 
          function(object){
            v_vel <- object@v
            temperature <-object@t
            v_vel <- cbind(v_vel,temperature)
            return(v_vel)
          }
          , sealed=FALSE)



#******************************************************************************
# S3 Perform the periodigram of a velocity vector
#******************************************************************************
dofft <- function(velocity,acq.freq){
  Npoint <- length(velocity)
  time <- Npoint*(1/acq.freq)
  ts <- seq(1/acq.freq,time,1/acq.freq-(1/1000000000))

  X.k <- fft(velocity)
  peaks <- Mod(X.k)/Npoint
  if(Npoint%%2==0){
    Freq1 <- seq(0, acq.freq/2, length.out=length(peaks)/2)
    Freq2 <- seq(acq.freq/2-acq.freq/Npoint, 0, length.out=length(peaks)/2)
    Freq <- c(Freq1, Freq2)
  }else{
    Freq1 <- seq(0, acq.freq/2, length.out=length(peaks)/2)
    Freq2 <- seq(acq.freq/2, 0, length.out=length(peaks)/2)
    Freq2 <- Freq2[-(length(Freq2)-1)]
    Freq <- c(Freq1, Freq2)
  }
  
  data_fft <- data.frame(fft_vel = X.k, freq = Freq, peaks = peaks, ts = ts)
  return(data_fft)
}

filter.data <- function(freq,fft_vel,fcut){
  index <- which(freq>fcut)
  
  fft_filt <- fft_vel
  fft_filt[index] <- 0+0i
  
  Npoint <- length(fft_vel)
  peaks <- Mod(fft_filt)/Npoint
  
  vel_filt <- fft(fft_filt, inverse=TRUE)/length(fft_filt)
  data_filt <- data.frame(freq = freq, fft_vel = fft_filt, peaks = peaks, vel=vel_filt)
  return(data_filt)
  
}

#******************************************************************************
# S3 method for casting an object into an object of class turbulence
#******************************************************************************

as.turbulence <- function(x, ...){
  UseMethod('as.turbulence')
}


as.turbulence.data.frame <- function(data){
  if(ncol(data)!=4) stop('Ncol is different from 4')
  if(ncol(data)==4){
    turb <- turbulence(u=data[,1], v=data[,2], w=data[,3], t=data[,4])
  }
  return(turb)
}


as.turbulence.matrix <- function(data){
  if(ncol(data)!=4) stop('Ncol is different from 4')
  if(ncol(data)==4){
    turb <- turbulence(u=data[,1], v=data[,2], w=data[,3], t=data[,4])
  }
  return(turb)
}

#******************************************************************************
# Default method: probably we can optimize the control on the length
#******************************************************************************

as.turbulence.default <- function(x, y, z, time){
  # If length of the elements passed differs, returns error (you must have the same number of data)
  if(length(u)!=length(v) | length(u)!=length(w) | length(u)!=length(t)
      | length(v)!=length(w) | length(v)!=length(t) | length(w)!=length(t)){
        stop('Error, lengths differ')
      }
  turb <- turbulence(u=x, v=y, w=z, t=time)
}



#******************************************************************************
# S3 method for casting an object of class turbulence into a data.frame
#******************************************************************************

as.data.frame.turbulence <- function(data){
  df <- data.frame(ncol=6, u=data@u, v=data@v, w=data@w, temp=data@t,
                   h_vel=data@h_vel, dir=data@dir)
  return(df)
}

