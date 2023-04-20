EDD_to_OKB <- function(df3) {
  #obtiene un dataframe que contiene las decisiones optimas de cada bloque
  
  #comprobamos que el argumento de entrada es un dataframe
  if(!is.data.frame(df3)){
    cat("Argument is not a dataframe \n")
    return (-1);
    
  }
    
   ## max by RR
   CU <- dim(df3)[2]
   CR <- dim(df3)[2]-1
   RR <- length( unique(df3[,CR])) 
   ri <- (1:dim(df3)[1]) %% RR == 1
   ri <- (1:dim(df3)[1])[ri]
   dfx <- data.frame()
   for( i in ri ) {
      r. <- i:(i+(RR-1))
      df. <- df3[r.,]
      k <- which( df.$Utility==max( df.$Utility))[1]
      dfx <- rbind( dfx,as.data.frame( df.[k,]))
      #cat(i,"<",k,">\n")
    }
   #eliminamos la primera columna y la última
   dfx <- dfx[,-1]
   dfx <- dfx[,ncol(dfx)]
    
   #creamos un fichero y guardamos el dataframe
   write.csv(dfx, file = "OptimalKB.csv")
    
   return (dfx);

}

#-----------------------pruebas---------------------
df3 <- read.csv(file="Terapia1.txt",sep=",",header=TRUE)
EDD_to_OKB(df3)
