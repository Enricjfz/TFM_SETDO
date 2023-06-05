fitness_function <- function(list_population,dfx,base,n) {
  #Fitness function of the kbm2l problem
  #Input:
  # - The list of the population (a dataframe)
  # - dfx the dataframe to be switched
  # - A vector of the base which belongs to the dataframe
  # - n: an integer which defines the selections of the best n population
  
  length_list <- dim(list_population)[1]
  if (n > length_list) {
    cat("La seleccion no puede ser mayor que la propia poblacion")
    return(-1)
  }
  items_vector <- c(1:length_list)
  for(i in 1:length_list) {
    items_vector[i] <- calculate_kbm2l_from_base(as.numeric(list_population[i,]),base,dfx)
  }
  
  
  list_population['items'] <- items_vector
  list_population <- list_population[order(list_population[[length(base)+1]]),] #or items
  
  return(list_population[1:n,1:length(base)])
  
}
