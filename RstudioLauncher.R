#working path from stdin
line <- readLines(stdin(), n=1)

# Defining some "macros" for Fontanella2/LiCor
if(line =="f2/LiCor" | line=="F2/LiCor" | line =="f2/licor" | 
     line =="F2/LiCor"){
  line <- "Fontanella2/LiCor"
}

path_dir <- paste(paste("data/",line, sep=""),"/", sep="")

cat(path_dir,"\n")

if(file.exists(path_dir)){
  cat("* Directory found...","\n")
} else {
  cat("* Directory not exists!","\n")
  quit()
}

source("main.R")
