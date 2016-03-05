# Here I try to define a S4 class for data of ultrasonic anemometer

# This is the definition of class turbulence
turbulence <- setClass('turbulence', slots=c(u='numeric', v='numeric', w='numeric', t='numeric'))


# S3 method for casting an object into an object of class turbulence: generic function
as.turbulence <- function(x, ...){
  UseMethod('as.turbulence')
}

# S3 methods for casting a dataframe/matrix into an object of class turbulence
# Now I must do the same for default and list
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

# as.turbulence.default <- function(u, v, w, t){
#   if(length(u)!=length(v) | length(u)!=length(w) | length(u)!=length(t)
#       | length(v)!=length(w) | length(v)!=length(t) | length(w)!=length(t)){
#         stop('Error, lengths differ')
#       }
# }