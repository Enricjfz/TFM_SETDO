library(bnlearn)
library(doParallel)
library(foreach)
library(dplyr)

#generate population using sample
generate_initial_population <- function(X,n) {
  # Generate the initial population used by the EDA
  # Inputs: 
  # X is an integer which defines the amount of population to be generated
  # n is an integer which defines the number of atributes of the decision problem
  list_polulation <- list()
  for(i in 1:X) {
    list_polulation[[i]] <- sample(1:n,n)
  }
  ret_dat_pop <- as.data.frame(do.call(rbind,list_polulation))
  return(ret_dat_pop)
  
}

#generate the population using the umda
generate_pop_umda <- function(M,X,k) {
  #M is a matrix of frequencies
  #X is the population to be generated
  #k is the number of atributes
  #returns a set of permutations
  
  P <- matrix(data=rep(0,k),1,k) ## poblacion
  for(x in 1:X){ ## ---> simular poblacion
    j <- 1
    while( j <= k) {
      perm <- rep(0,k)
      while( min(perm)==0) {
        U <- runif(k,0,1)
        U <- U / sum(U)
        #cat("U  ",U,"\n")
        atti <- c()
        weigth <- c()
        for( i in 1:k) {
          #cat("U[i] < M[i,j]", U[i]," < ", M[i,j],"\n")
          if ( U[i] < M[i,j]) {
            atti <- c(atti, i)
            weigth <- c(weigth, U[i])
          }
        }
        if (length(weigth)>0) {   
          ow <- order(weigth)
          for( a in 0:(length(ow)-1)) {
            att <- atti[ow[length(ow)-a]]
            if (!(att %in% perm)) {
              perm[j] <- att; j<-j+1;break
            }
          }}
        #cat("P  ",perm,"\n")   
      }    
      ## restaurar permutacion
      #print(perm)    
      P<- rbind(P,perm)
      #print(sort(perm))    
    }
  }
  P <- P[-c(1),]
  #P <- as.data.frame(P)
  return(P)
}

generate_pop_umda_parallel <- function(M,X,k) {
  #M is a matrix of frequencies
  #X is the population to be generated
  #k is the number of atributes
  #returns a set of permutations
  
  num_cores <- 4
  #Start the parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  P <- matrix(data = rep(0, X * k), nrow = X, ncol = k) ## poblacion
  P <- foreach( x = 1:X, .combine = rbind) %dopar%{ ## ---> simular poblacion
    j <- 1
    while( j <= k) {
      perm <- rep(0,k)
      while( min(perm)==0) {
        U <- runif(k,0,1)
        U <- U / sum(U)
        #cat("U  ",U,"\n")
        atti <- c()
        weigth <- c()
        for( i in 1:k) {
          #cat("U[i] < M[i,j]", U[i]," < ", M[i,j],"\n")
          if ( U[i] < M[i,j]) {
            atti <- c(atti, i)
            weigth <- c(weigth, U[i])
          }
        }
        if (length(weigth)>0) {   
          ow <- order(weigth)
          for( a in 0:(length(ow)-1)) {
            att <- atti[ow[length(ow)-a]]
            if (!(att %in% perm)) {
              perm[j] <- att; j<-j+1;break
            }
          }}
        #cat("P  ",perm,"\n")   
      }    
      P[x, ] <- perm
    }
  }
  # Stop the parallel backend
  stopCluster(cl)
  return(P)
}

reemplazar_elementos_repetidos <- function(vector) {
  if (length(unique(vector)) == length(vector)) {
    # El vector no tiene elementos repetidos, devolvemos el mismo vector
    return(vector)
  } else {
    # El vector tiene elementos repetidos
    duplicated = duplicated(vector) #vector de booleanos
    unique_values = c(1:length(vector))
    unused_elements = setdiff(unique_values,vector)
    vector[duplicated] = unused_elements
    return(vector)
  }
}


generate_new_pop_tan <- function(perm,n) {
  #convertir el dataframe a uno de factores
  perm <-data.frame(lapply(perm,factor))
  
  # Construir el modelo TAN con los mejores individuos
  tan <- tree.bayes(x=perm,training = 'items')
  
  # Generar nuevas soluciones utilizando el modelo TAN
  new_pop <- rbn(tan, n = n,data=perm)
  
  #Eliminamos la columna predictora
  new_pop <- new_pop[,-1]
  
  #Pasamos de nuevo el dataframe de factores a numericos
  new_pop <- data.frame(lapply(new_pop,as.numeric))
  
  #Restauramos las permutaciones
  
  new_rest_pop <- apply(new_pop,1,reemplazar_elementos_repetidos)
  
  return(as.data.frame(t(new_rest_pop)))
}
