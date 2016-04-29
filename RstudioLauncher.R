#working path from stdin
line <- readLines(stdin(), n=1)

# Defining some "macros" for Fontanella2/LiCor
if(line =="f2/LiCor" | line=="F2/LiCor" | line =="f2/licor" | 
     line =="F2/LiCor"){
  line <- "Fontanella2/LiCor"
}

# Defining some "macros" for Fontanella1/LiCor
if(line =="f1/LiCor" | line=="F1/LiCor" | line =="f1/licor" | 
     line =="F1/LiCor"){
  line <- "Fontanella1/LiCor"
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
