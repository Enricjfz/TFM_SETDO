library(mvtnorm)
#use a normal distribution to generate new population
calculate_new_pop <- function(fitness_pop,k,n) {
  #inputs: 
  #fitness pop: a dataframe containing the fitness population
  #k: is the number of atributes
  #n: is the number of population to be generated
  colMean <- colMeans(fitness_pop)
  colCov <- cov(fitness_pop)
  
  
  new_pop <- rmvnorm(n, mean = colMean, sigma =colCov)
  new_pop <- round(new_pop)
  new_pop <- pmax(pmin(new_pop, k), 1)
  
  return(new_pop)
}

#update umda distribution with the new fitness population
update_umda <- function(M,perm,k) {
  #M is a matrix of frequencies
  #Perm is a set of permutations which are the best ones according to the fitness function
  #k is the number of atributes of the problem
  #returns the updated M
  perm <- as.matrix(perm)
  for( j in 1:k) {
    fP <- factor(P[,j],levels=1:k) #actualizacion de M + ruido evitar optimos locales (evitar 0)
    M[,j] <- table(fP) + abs(rnorm(k))/k 
    M[,j] <- M[,j] / sum(M[,j])
  }
  return(M)
}

#function which calculates the matrix of frequencies
calculate_initial_univariate_distribution <-function(k) {
  #k is the number of atributes of the problem
  #returns M which is a matrix of frequencies
  
  M <- matrix(data=rep(0,k*k),k,k)
  for( j in 1:k) { 
    m1 <- (sample(1:k,k))
    m1 <- m1 / sum(m1)
    M[,j] <- m1 #inicializacion equiprobabble
  }
  return(M)
}
