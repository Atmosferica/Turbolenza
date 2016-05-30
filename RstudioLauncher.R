
#working path from stdin
cat("cartella\n")
line <- readLines(stdin(), n=1)
cat("frequenza (20 per i LiCor, 10 per gli altri)\n")
sonic_fqc <- as.numeric(readLines(stdin(), n=1))

cat("Minuti minimi di campionamento\n")
min_camp <- as.numeric(readLines(stdin(), n=1))

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

# Defining some "macros" for Fontanella1/Control
if(line =="f1/control" | line=="F1/control" | line =="f1/Control" | 
     line =="F1/Control"){
  line <- "Fontanella1/Control"
}


# Defining some "macros" for Fontanella1/Control
if(line =="f2/control" | line=="F2/control" | line =="f2/Control" | 
   line =="F2/Control"){
  line <- "Fontanella2/Control"
}

# Defining some "macros" for Fontanella2/Emission
if(line =="f2/Emission" | line=="F2/Emission" | line =="f2/emission" | 
     line =="F2/emission"){
  line <- "Fontanella2/Emission"
}

# Defining some "macros" for Fontanella1/Emission
if(line =="f1/Emission" | line=="F1/Emission" | line =="f1/emission" | 
     line =="F1/emission"){
  line <- "Fontanella1/Emission"
}

path_dir <- paste(paste("data/",line, sep=""),"/", sep="")

cat(path_dir,"\n")

# if(file.exists(path_dir)){
#   cat("* Directory found...","\n")
# } else {
#   cat("* Directory not exists!","\n")
#   quit()
# }

source("main.R")
