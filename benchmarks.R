library(microbenchmark)
library(ggplot2)
library(tidyr)

#benchmarks y gráficos

# Measure the execution time of a function
find_best_base1 <- microbenchmark(
  # Function or expression to measure execution time
  expr = {
    get_optimal_base(dfx_pruebas,c(1,2,3,4,5,6))
  },
  times = 20  # Number of times to repeat the measurement
)
print(find_best_base1)

find_best_base2 <- microbenchmark(
  
  expr = {
    eda_kbm2l(dfx_pruebas,c(1,2,3,4,5,6),150,50,25) #sin opt
  },
  times = 20
)
print(find_best_base2)

find_best_base3 <- microbenchmark(
  
  expr = {
    eda_kbm2l(dfx_pruebas,c(1,2,3,4,5,6),150,50,25) #opt
  },
  times = 40
)
print(find_best_base3)

find_best_base4 <- microbenchmark(
  
  expr = {
    get_optimal_base(dfx_nhlv2,c(1,2,3,4,5,6,7,8)) #sin optimizar
  },
  times = 20
)
print(find_best_base4)

find_best_base5 <- microbenchmark(
  
  expr = {
    get_optimal_base(dfx_nhlv2,c(1,2,3,4,5,6,7,8)) #optimizada
  },
  times = 20
)
print(find_best_base5)

find_best_base6 <- microbenchmark(
  
  expr = {
    eda_kbm2l(dfx_nhlv2,c(1,2,3,4,5,6,7,8),150,50,25) # sin opt
  },
  times = 20
)
print(find_best_base6)

#TAN

find_best_base7 <- microbenchmark(
  
  expr = {
    eda_tan_kbm2l(dfx_pruebas,c(1,2,3,4,5,6),150,50,25) #sin optimizar
  },
  times = 20
)
print(find_best_base7)

find_best_base8 <- microbenchmark(
  
  expr = {
    eda_tan_kbm2l(dfx_pruebas,c(1,2,3,4,5,6),150,50,25) #optimizado
  },
  times = 20
)
print(find_best_base8)

find_best_base9 <- microbenchmark(
  
  expr = {
    eda_tan_kbm2l(dfx_nhlv2,c(1,2,3,4,5,6,7,8),150,50,25) #sin optimizar
  },
  times = 20
)
print(find_best_base9)

find_best_base10 <- microbenchmark(
  
  expr = {
    eda_kbm2l(dfx_pruebas,c(1,2,3,4,5,6),150,50,25) #paralelo ---> MUY COSTOSO TARDA 1H
  },
  times = 20
)
print(find_best_base10)

find_best_base11 <- microbenchmark(
  
  expr = {
    eda_kbm2l(dfx_nhlv2,c(1,2,3,4,5,6,7,8),150,50,25) #opt
  },
  times = 20
)

print(find_best_base11)

find_best_base12 <- microbenchmark(
  
  expr = {
    eda_tan_kbm2l(dfx_nhlv2,c(1,2,3,4,5,6,7,8),150,50,25) #nhvl sin opt
  },
  times = 20
)
print(find_best_base12)

find_best_base13 <- microbenchmark(
  
  expr = {
    eda_tan_kbm2l(dfx_nhlv2,c(1,2,3,4,5,6,7,8),150,50,25) #nhvl opt
  },
  times = 20
)
print(find_best_base13)



####--------------graficas---------------###

dfx_speed <- data.frame("metodo" = c("Exhaustivo","Exh_opt","EDA","EDA_pal","EDA_mem","EDA+TAN","EDA+TAN_opt"), "maximo" = c(23.21,20.80,53940,243930,3530,226.8,126.5), "media" = c(13.66,10.53,29410,225360,12610,129.6,86.09), "minimo" = c(9.81,7.40,22670,215370,9510,87.11,39.16))

dfx_speed2 <- data.frame("metodo" = c("Exhaustivo","Exh_opt","EDA","EDA_mem","EDA+TAN","EDA+TAN_opt"), "maximo" = c(367.87,318.08,80990,72970,223.23,203.73), "media" = c(319.89,286.21,31200,29770,184.22,166.77), "minimo" = c(306.42,276.73,24330,23990,171.15,156.19))


colores <- c("red", "blue", "green")

# Crear la gráfica vacía con los ejes y la leyenda
plot(1, 1, type = "n", xlim = c(1, nrow(dfx_speed2)), ylim = log10(range(dfx_speed2$maximo, dfx_speed2$media, dfx_speed2$minimo)),
     xlab = "Métodos", ylab = "Tiempos (log10)", main = "Tiempos de ejecución por método (grande)")

# Establecer las etiquetas de los métodos en el eje X
#axis(side = 1, at = 1:nrow(dfx_speed), labels = dfx_speed$metodo, tick = FALSE)


# Agregar las líneas para los tres tiempos con transformación logarítmica
lines(1:nrow(dfx_speed2), log10(dfx_speed2$maximo), col = colores[1])
lines(1:nrow(dfx_speed2), log10(dfx_speed2$media), col = colores[2])
lines(1:nrow(dfx_speed2), log10(dfx_speed2$minimo), col = colores[3])

# Ajustar el tamaño de fuente de la leyenda
legend("topright", legend = c("Máximo", "Media", "Mínimo"), col = colores, lty = 1, cex = 0.4)


ggplot(data = dfx_speed2, aes(x = metodo, y = c(maximo,media,minimo), color = metodo)) +
  geom_line() +
  labs(x = "Métodos", y = "Tiempos", title = "Tiempos de ejecución por método") +
  theme_minimal() +
  theme(legend.position = "right")
