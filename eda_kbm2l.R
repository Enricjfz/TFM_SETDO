eda_kbm2l <- function(dfx,base_inicial,iter,pop,sel) {
  #Function which uses the EDA algorithm to calculate the optimal base
  #dfx is the initial dataframe of the decision problem
  #base_inicial is the initial base
  #iter is the number of iterations of the problem
  #pop is the population to be generated
  #sel is the selection of the population after the fitness
  
  attr <- length(base_inicial)
  #initial_pop <- generate_initial_population(pop,attr)
  #new_pop <- initial_pop
  M <- calculate_initial_univariate_distribution(attr)
  for(i in 1:iter) {
    new_pop <- generate_pop_umda(M,pop,attr)
    fitness_pop <- fitness_function(new_pop,dfx,base_inicial,sel)
    #new_pop <- calculate_new_pop(fitness_pop,attr,pop)
    M <- update_umda(M,fitness_pop,attr)
  }
  return(fitness_pop)
}
