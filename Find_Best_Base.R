get_optimal_base <- function(dfx, base_inicial) {
  attr <- length(base_inicial)
  if(dim(dfx)[2]-1 != attr) {
    cat("wrong initial base\n")
    return(-1)
  }
  
  lista_bases_visitadas <- list()
  indice_lista_actual <- 1
  lista_bases_visitadas[[indice_lista_actual]] <- base_inicial
  indice_lista_actual <- indice_lista_actual + 1
  #n_bases <- factorial(attr)
  best_perm <- Find_Best_Base(dfx,base_inicial,lista_bases_visitadas,indice_lista_actual)
  return(best_perm)
  
}

Find_Best_Base <- function(dfx, base_actual,lista_bases, index_lista) {
  
  n_col <- dim(dfx)[2] -1 #no usamos la columna de decisiones
  p_perm <- base_actual #posible permutacion
  best_perm <- base_actual #mejor permutacion
  dfx_inicial <- dfx #kb original
  best_dfx <- dfx
  stop_condition <- 0
    
  kbm2l_actual <- reduce(dfx[[n_col+1]]) #obtenemos la kmb2l inicial
  
  for (i in 1:n_col) {
    for(j in 1:n_col) {
      if(i == j) {
        next #no permutation
      }
      #swap base, se comprueba si esta en la lista
      back <- p_perm[i]
      p_perm[i] <- p_perm[j]
      p_perm[j] <- back
      
      if (Position(function(x) identical(x, p_perm), lista_bases, nomatch = 0) > 0) {
        #esta en la lista, se salta iteración
        p_perm <- base_actual
        next
      }
      
      lista_bases[[index_lista]] <- p_perm
      index_lista <- index_lista + 1
      dfx_swapped <- swap_columns_order(dfx,i,j)
      new_kbm2l <- reduce(dfx_swapped[[n_col +1]])
      if(dim(new_kbm2l)[1] < dim(kbm2l_actual)[1]) {
        #hemos encontrado una base mejor
        best_dfx <- dfx_swapped
        kbm2l_actual <- new_kbm2l
        best_perm <- p_perm
        stop_condition <- 1
      }
      
      p_perm <- base_actual #volvemos a la base inicial
      
      
    }
   
  }
  if(stop_condition) {
    #hemos encontrado una base mejor, se sigue iterando
    return(Find_Best_Base(best_dfx,best_perm,lista_bases,index_lista))
    
  }
  else {
    #hemos encontrado una base mejor, supuesto optimo
    return(best_perm)
  }
  
}


