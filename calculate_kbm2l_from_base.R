calculate_kbm2l_from_base <- function(to_base,original_base,dfx) {
  #check if to base is equals to original_base
  if(length(to_base) != length(original_base)) {
    cat("Bases must have the same length\n")
    return(-1)
  }
  #loops until original base is equals to to_base
  attr <- length(to_base)
  new_dfx <- dfx
  while(identical(to_base,original_base) == FALSE) {
    for(i in 1:attr) {
      if(original_base[i] != to_base[i]) {
        for(j in 1:attr) {
          if(original_base[i] == to_base[j]) {
            new_dfx <- swap_columns_order(new_dfx,i,j,0)
            back <- original_base[i] 
            original_base[i] <- original_base[j]
            original_base[j] <- back
            break
          }
        }
      }
    }
  }
  
  new_dfx <- new_dfx[order(new_dfx[[1]]),]
  
  #print(new_dfx)
  
  kbm2l <- reduce(new_dfx[[attr+1]])
  
  items <- dim(kbm2l)[1]
  
  return(items)
  
}

