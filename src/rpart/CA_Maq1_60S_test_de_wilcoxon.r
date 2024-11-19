# APLICANDO TEST DE WILCOX

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

# Se presentan los vectores de 20 semillas, correspondientes al envio de mayor ganancia promedio para cada método:

# Machine Learning (ML) ---> Reemplaza Ceros por NA
ML40S_2000 <- c(5.319, 7.389, 6.669, 6.989, 6.109, 6.229, 6.139, 6.219, 5.409,
                6.909, 6.949, 6.209, 7.509, 8.159, 7.189, 6.569, 6.229, 7.329,
                6.629, 7.159, 6.989, 6.959, 7.129, 6.409, 6.689, 6.969, 6.859,
                5.429, 7.499, 5.709, 7.289, 6.499, 7.809, 5.919, 7.049, 8.219,
                7.079, 6.459, 6.919, 7.149)

#No hacer nada (Ninguno) ---> Mantiene los CEROS
Ninguno40S_2000 <- c(6.929,6.949,7.279,6.969,7.399,7.059,6.619,7.829,6.669,7.749,
                  6.889,6.229,7.009,7.789,6.129,6.509,6.119,6.919,7.399,8.169,
                  6.709,7.759,7.399,6.179,6.569,7.339,6.879,6.879,6.899,7.259,
                  6.549,6.569,6.789,6.259,7.049,7.049,6.059,6.139,6.899,7.339)

#Estadistica Clasica (EC) ---> Imputa por la media
EC40S_1800 <- c(5.559,6.459,6.009,6.809,5.319,4.849,5.729,5.969,5.339,5.919,5.949,
             5.709,5.069,5.599,5.939,5.679,5.999,6.099,5.609,7.769,5.639,6.399,
             6.779,6.959,5.269,5.679,5.529,5.589,5.869,4.619,5.959,6.269,5.709,
             6.889,5.899,5.799,5.969,6.129,5.639,6.489)

# MICE ---> Imputa por el metodo Imputacion Multiple con Ecuaciones Encadenadas
MICE40S_2400 <- c(6.149,6.559,5.969,6.659,5.319,5.789,4.969,6.649,7.109,5.959,7.399,
               6.709,5.729,5.519,7.039,6.599,6.539,6.089,5.079,5.479,6.629,6.739,
               6.659,6.499,6.239,6.979,5.839,5.689,7.889,7.759,5.439,5.949,6.259,
               5.269,4.619,6.569,7.279,5.559,6.719,7.749)

#vector de ganancias promedio obtenidas de aplicar ...

#Realizar el test de Wilcoxon para cada par de vectores

combinaciones <- combn(list( ML40S_2000, Ninguno40S_2000, EC40S_1800, MICE40S_2400), 2, simplify = FALSE)
nombres_vectores <- c("ML40S_2000", "Ninguno40S_2000", "EC40S_1800", "MICE40S_2400")

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
  Ganancia = c(ML40S_2000, Ninguno40S_2000, EC40S_1800, MICE40S_2400),
  Metodo = factor(rep(nombres_vectores, times = c(length(ML40S_2000), length(Ninguno40S_2000), length(EC40S_1800), length(MICE40S_2400))))
)

library(ggplot2)
ggplot(datos, aes(x = Metodo, y = Ganancia, fill = Metodo)) +
  geom_boxplot() +
  labs(title = "Comparación de Ganancias Promedio entre Vectores", x = "Metodo", y = "Ganancia Promedio") +
  theme_minimal() +
  theme(legend.position = "none")