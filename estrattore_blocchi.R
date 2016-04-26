signal.block.mean <- function(time.stamp, signal, block.length=300) {
  time.index <- time.stamp %/% block.length
  part <- aggregate(signal, by=list(time.index), FUN=mean, na.rm=TRUE)
  names(part) <- c("time.stamp", "value")
  part$time.stamp <- part$time.stamp * block.length
  return(part)
}

# time.stamp = vector of times (from 0s to 3600s if we consider an hour)
# block = index of the cycle, number of considered block
# signal = dataset 
# block.length = length of the result
# blocks are not overlied! Need to compute first the number of blocks...

signal.partition <- function(time.stamp, signal, block, block.length=300) {
  time.index <- time.stamp %/% block.length
  separate.indices <- unique(time.index)
  n.blocks <- max(separate.indices)
  if(block <= 0 | block > n.blocks) return(NULL)
  current.block.idx <- which(time.index == separate.indices[block])
  current.block <- signal[current.block.idx]
  current.time.stamp <- time.stamp[current.block.idx]
  result <- data.frame(time.stamp = current.time.stamp, value = current.block)
  return(result)
}
