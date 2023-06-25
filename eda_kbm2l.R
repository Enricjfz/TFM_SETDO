eda_kbm2l <- function(dfx,base_inicial,iter,pop,sel) {
  #Function which uses the EDA algorithm to calculate the optimal base
  #dfx is the initial dataframe of the decision problem
  #base_inicial is the initial base
  #iter is the number of iterations of the problem
  #pop is the population to be generated
  #sel is the selection of the population after the fitness
  
  #start <- Sys.time()
  attr <- length(base_inicial)
  #initial_pop <- generate_initial_population(pop,attr)
  #new_pop <- initial_pop
  M <- calculate_initial_univariate_distribution(attr)
  for(i in 1:iter) {
    new_pop <- generate_pop_umda(M,pop,attr)
    fitness_pop <- fitness_function(new_pop,dfx,base_inicial,sel,iter)
    M <- update_umda(M,fitness_pop,attr)
  }
  #print(Sys.time() - start)
  return(fitness_pop)
}


eda_tan_kbm2l <- function(dfx,base_inicial,iter,pop,sel) {
  #Function which uses the EDA algorithm to calculate the optimal base
  #dfx is the initial dataframe of the decision problem
  #base_inicial is the initial base
  #iter is the number of iterations of the problem
  #pop is the population to be generated
  #sel is the selection of the population after the fitness  
  attr <- length(base_inicial)
  population <- generate_initial_population(pop,attr)
  for(i in 1:iter) {
    fitness_pop <- fitness_function_tan(population,dfx,base_inicial,sel)
    #categorizar la columna predictora (items kbm2l)
    #fitness_pop['items'] <- cut(fitness_pop[['items']],breaks = c(0,30,50,70,100,120,150,Inf),labels = c(1,2,3,4,5,6,7))
    fitness_pop['items'] <- cut(fitness_pop[['items']],breaks = c(0,600,750,800,850,900,950,Inf),labels = c(1,2,3,4,5,6,7))
    #print(length(unique(fitness_pop$items)))
    if(length(unique(fitness_pop$items)) == 1)
    {
      #se ha convergido a un valor
      #print("Iter")
      #print(i)
      return(fitness_pop)
    }
    population <- generate_new_pop_tan(fitness_pop,pop)
  }
  return(population)
}

#eda_kbm2l(dfx_pruebas,c(1,2,3,4,5,6),150,50,25)
#eda_kbm2l(dfx_nhlv2,c(1,2,3,4,5,6,7,8),300,50,25)
#eda_tan_kbm2l(dfx_pruebas,c(1,2,3,4,5,6),50,50,25)
#eda_tan_kbm2l(dfx_nhlv2,c(1,2,3,4,5,6,7,8),150,100,25)

