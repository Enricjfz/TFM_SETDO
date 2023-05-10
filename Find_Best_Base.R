Find_Best_Base <- function(dfx) {
  
  n_col <- dim(dfx)[2] -1 #no usamos la columna de decisiones
  base_inicial <- c(1:n_col) # base inicial
  p_perm <- base_inicial #posible permutacion
  best_perm <- base_inicial #mejor permutacion
  lista_bases <- list() #lista de permutaciones visitadas
  index_lista <- 1
  lista_bases[[index_lista]] <- base_inicial
  index_lista <- index_lista + 1
  dfx_inicial <- dfx #kb original
    
  kbm2l_actual <- reduce(dfx[[n_col+1]]) #obtenemos la kmb2l inicial
  
  for (i in 1:n_col) {
    for(j in 1:n_col) {
      if(i == j) {
        next #no permutation
      }
      #swap base, se comprueba si esta en la lista
      p_perm[i] <- j
      p_perm[j] <- i
      
      if (Position(function(x) identical(x, p_perm), lista_bases, nomatch = 0) > 0) {
        #esta en la lista, se salta iteración
        p_perm <- base_inicial
        next
      }
      
      lista_bases[[index_lista]] <- p_perm
      index_lista <- index_lista + 1
      dfx_swapped <- swap_columns_order(dfx,i,j)
      new_kbm2l <- reduce(dfx_swapped[[n_col +1]])
      if(dim(new_kbm2l)[1] < dim(kbm2l_actual)[1]) {
        #hemos encontrado una base mejor
        kbm2l_actual <- new_kbm2l
        best_perm <- p_perm
      }
      
      p_perm <- base_inicial #volvemos a la base inicial
      dfx <- dfx_inicial
      
    }
   
  }
  return(kbm2l_actual)
  
}


