reduce <- function(arr) {
  n <- length(arr)
  
  if (n <= 2) {
    return(arr)  # No se puede reducir más el vector
  }
  
  idx <- 1  # Índice actual en el vector reducido
  reduced_arr <- c(arr[1])  # Vector reducido
  
  for (i in 2:n) {
    # Si el elemento actual es distinto de sus vecinos, añadirlo al vector reducido
    if (arr[i] != arr[i - 1]) {
      cat("offset : ",i-1,"\n")
      reduced_arr[idx] <- arr[i-1]
      idx <- idx + 1
    }
  }
  
  reduced_arr <- c(reduced_arr, arr[n])  # Añadir el último elemento
  
  return(reduced_arr)
}

# Pruebas
arr <- c(2, 2, 3, 4, 5, 5)
reduced_arr <- reduce(arr)
cat("Vector reducido:", reduced_arr, "\n")
