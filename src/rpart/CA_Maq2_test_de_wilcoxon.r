# APLICANDO TEST DE WILCOX

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

# Se presentan los vectores de 20 semillas, correspondientes al envio de mayor ganancia promedio para cada método:

# Machine Learning (ML) ---> Reemplaza Ceros por NA
ML_1600 <- c()

#No hacer nada (Ninguno) ---> Mantiene los CEROS
Ninguno_2000 <- c(6.929, 6.949,	7.279,	6.969,	7.399,	7.059,	6.619,	7.829,	6.669,	7.749,	6.889,	6.229,	7.009,
                  7.789,	6.129,	6.509,	6.119,	6.919,	7.399)

#Estadistica Clasica (EC) ---> Imputa por la media
EC_1800 <- c(5.559,	6.459,	6.009,	6.809,	5.319,	4.849,	5.729,	5.969,	5.339,	5.919,	5.949,	5.709,	5.069,
             5.599, 5.939,	5.679,	5.999,	6.099,	5.609)

# MICE ---> Imputa por el metodo Imputacion Multiple con Ecuaciones Encadenadas
MICE_2200 <- c(5.679,	6.769,	6.229,	6.749,	5.859,	5.909,	5.149,	6.779,	7.299,	6.269,	7.549,	7.309,	5.509,
               5.289,	6.379,	6.299,	6.789,	5.899,	5.549)

#vector de ganancias promedio obtenidas de aplicar ...

#Realizar el test de Wilcoxon para cada par de vectores

combinaciones <- combn(list( ML_1600, Ninguno_2000, EC_1800, MICE_2200), 2, simplify = FALSE)
nombres_vectores <- c("ML_1600", "Ninguno_2000", "EC_1800", "MICE_2200")

resultados_wilcox <- lapply(combinaciones, function(par) {
  test <- wilcox.test(par[[1]], par[[2]], paired = FALSE)  # Cambia a `TRUE` si los vectores están relacionados
  return(test$p.value)
})

# Mostrar los resultados del p-value para cada par
nombres_pares <- combn(nombres_vectores, 2, paste, collapse = " vs ")
names(resultados_wilcox) <- nombres_pares
print(resultados_wilcox)

# Paso 2: Crear un gráfico boxplot para comparar los vectores
datos <- data.frame(
  Ganancia = c(ML_1600, Ninguno_2000, EC_1800, MICE_2200),
  Metodo = factor(rep(nombres_vectores, times = c(length(ML_1600), length(Ninguno_2000), length(EC_1800), length(MICE_2200))))
)

library(ggplot2)
ggplot(datos, aes(x = Metodo, y = Ganancia, fill = Metodo)) +
  geom_boxplot() +
  labs(title = "Comparación de Ganancias Promedio entre Vectores - Maq 2", x = "Metodo", y = "Ganancia Promedio") +
  theme_minimal() +
  theme(legend.position = "none")