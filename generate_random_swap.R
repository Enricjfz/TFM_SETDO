generate_random_swap<- function(n_cols) {
  aleatorio1 <- sample(1:n_cols, 1)
  aleatorio2 <- sample(1:n_cols, 1)
  while(aleatorio1 == aleatorio2) {
    #nos cercionamos de que aleatorio1  y aleatorio2 sean distintos
    aleatorio2 <- sample(1:n_cols, 1)
  }
  return(c(aleatorio1, aleatorio2))
  
}
