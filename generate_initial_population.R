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
  for( x in 1:X) { ## ---> simular poblacion
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
  P <- as.data.frame(P)
  return(P)
}
