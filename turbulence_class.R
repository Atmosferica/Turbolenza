# Here I try to define a S4 class for data of ultrasonic anemometer

# This is the definition of class turbulence
turbulence <- setClass('turbulence', slots=c(u='numeric', v='numeric', w='numeric', t='numeric'))

setGeneric('get_hvel', function(object){ # To define an S4 method, I must create a generic first
  standardGeneric('get_hvel')       # standardGeneric is the S4 equivalent of UseMethod
})

setMethod('get_hvel', signature='turbulence', 
          function(object){
            x_vel <- object@u # wind x velocity (west to east)
            y_vel <- object@v # wind y velocity (south to north)
            h_vel <- c(1:length(x_vel))
            h_vel <- sqrt(x_vel^2 + y_vel^2) # horizontal velocity
            return(h_vel*2) #Perche` ritorna moltipicata per 2?
}
          , sealed=FALSE)



# S3 method for casting an object into an object of class turbulence
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


# Default method: probably we can optimize the control on the length
as.turbulence.default <- function(x, y, z, time){
  # If length of the elements passed differs, returns error (you must have the same number of data)
  if(length(u)!=length(v) | length(u)!=length(w) | length(u)!=length(t)
      | length(v)!=length(w) | length(v)!=length(t) | length(w)!=length(t)){
        stop('Error, lengths differ')
      }
  turb <- turbulence(u=x, v=y, w=z, t=time)
}


