#working path from stdin
line <- readLines(stdin(), n=1)
path_dir <- paste(paste("data/",line, sep=""),"/", sep="")

cat(path_dir,"\n")

if(file.exists(path_dir)){
  cat("* Directory found...","\n")
} else {
  cat("* Directory not exists!","\n")
  quit()
}

source("main.R")
