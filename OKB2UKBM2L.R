OptimalKB_to_UKBM2L <- function(dfx) {
  #comprobamos que el argumento de entrada es un dataframe
  if(!is.data.frame(dfx)){
    return (-1); 
    
  }
  #creamos un nuevo fichero con los nombres de las columnas
  file <- "headers.txt"
  if(file.exists(file)) {
    cat("file already exists, deleting old one \nco")
    file.remove(file)
    cat("creating file columns \n")
    file.create(file)
  }
  else {
    cat("creating file columns \n")
    file.create(file)
  }
  
  colNames <- colnames(dfx)
  colNames <- colNames[3:length(colNames)-1] #no queremos la primera columna
  colNames <- t(colNames) #transponemos
  write(colNames,file = file,append=FALSE, sep=" ")
  
  #Se crea la lista en forma de matriz (decision,offset) 
  
  decision <- dfx[[ncol(dfx)]] #optimal decision
  mat1 <- reduce(decision)
  
  #Se crea otro fichero que tiene la UKBM2L 
  
  
  file2 <- "base_knm2l.txt"
  if(file.exists(file2)) {
    cat("file already exists, deleting old one \n")
    file.remove(file2)
    cat("creating file base \n")
    file.create(file2)
  }
  else {
    cat("creating file base \n")
    file.create(file2)
  }
  
  #escribimos en el fichero
  write.table(mat1,file = file2,sep=",")
}

#pruebas de la funcion

dfx <- read.csv("OptimalKB.csv")
OptimalKB_to_UKBM2L(dfx)

