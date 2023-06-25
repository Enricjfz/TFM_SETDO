library(doParallel)
library(foreach)

fitness_function <- function(list_population,dfx,base,n,print) {
  #Fitness function of the kbm2l problem
  #Input:
  # - The list of the population (a matrix)
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
    items_vector[i] <- calculate_kbm2l_from_base(c(list_population[i,]),base,dfx)
    #items_vector[i] <- calculate_kbm2l_from_base(as.numeric(list_population[i,]),base,dfx)
  }
  
  list_population <- cbind(list_population,items_vector)
  list_population <- list_population[order(list_population[,length(base)+1],decreasing=FALSE),]
  #list_population['items'] <- items_vector
  #list_population <- list_population[order(list_population[[length(base)+1]]),] #or items
  if(print == 150) {
    #print(list_population[,length(base)+1])
  }
  return(list_population[1:n,1:length(base)])
  
}

fitness_function_tan <- function(list_population,dfx,base,n) {
  #Fitness function of the kbm2l problem
  #Input:
  # - The list of the population (a matrix)
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
  
  return(list_population[1:n,])
  
}

fitness_function_parallel <- function(list_population,dfx,base,n) {
  #Fitness function of the kbm2l problem
  #Input:
  # - The list of the population (a matrix)
  # - dfx the dataframe to be switched
  # - A vector of the base which belongs to the dataframe
  # - n: an integer which defines the selections of the best n population
  
  length_list <- dim(list_population)[1]
  if (n > length_list) {
    cat("La seleccion no puede ser mayor que la propia poblacion")
    return(-1)
  }
  items_vector <- c(1:length_list)
  num_cores <- 6
  #Start the parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    source("C:/Users/Enrique/Desktop/Enrique/Universidad/master/TFM/TFM_SETDO/calculate_kbm2l_from_base.R") 
    source("C:/Users/Enrique/Desktop/Enrique/Universidad/master/TFM/TFM_SETDO/swap_columns_order.r")
    source("C:/Users/Enrique/Desktop/Enrique/Universidad/master/TFM/TFM_SETDO/reduce_kbm2l.R")
  })
  items_vector <-foreach(i = 1:length_list, .combine = c) %dopar% {
     calculate_kbm2l_from_base(c(list_population[i,]),base,dfx)
  }
  # Stop the parallel backend
  stopCluster(cl)
  
  list_population <- cbind(list_population,items_vector)
  list_population <- list_population[order(list_population[,length(base)+1],decreasing=FALSE),]
 
  return(list_population[1:n,1:length(base)])
  
}
