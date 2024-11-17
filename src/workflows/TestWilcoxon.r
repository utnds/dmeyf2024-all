# APLICANDO TEST DE WILCOX


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection


# Dados los siguientes vectores:


#vector obtenido de obtenidas de aplicar "MACHINE LEARNING" (envio 1800)
ML_1800 <- c(6.849,	6.909,	4.949,	5.639,	5.909,	6.849,	7.289,	7.019,	7.869,	7.069,	7.899,	5.499,	7.649,	7.169,	5.199,	5.989,	6.039,	7.939,	7.329,	5.539) 


#vector obtenido al aplicar "NINGUNO" (envío 1800)
Ninguno_1800<- c(4.769,	6.339,	6.629,	6.829,	5.939,	5.129,	4.659,	6.019,	6.129,	7.219,	5.189,	7.319,	6.879,	5.189,	5.439,	5.209,	6.659,	6.039,	6.399,	7.399) 

#vector obtenido de aplicar "ESTADICTICA CLASICA" (envio 2200)
EC_2200 <- c(3.209,	3.569,	5.769,	5.849,	5.099,	4.239,	4.139,	6.279,	4.959,	5.389,	4.549,	6.269,	5.149,	4.739,	3.509,	3.099,	4.829,	3.789,	4.589,	6.739)


#vector obtenido de aplicar "MICE" (envío 1600)
MICE_1600 <- c(3.569,	6.879,	5.399,	6.059,	3.429,	4.739,	3.989,	4.479,	4.909,	5.779,	4.899,	5.339,	5.649,	5.019,	6.309,	5.879,	6.129,	5.659,	2.019,	4.659) 


#Realizar el test de Wilcoxon para cada par de vectores


combinaciones <- combn(list(ML_1800, Ninguno_1800, EC_2200 , MICE_1600), 2, simplify = FALSE)
nombres_vectores <- c("ML_1800", "Ninguno_1800", "EC_2200", "MICE_1600")

resultados_wilcox <- lapply(combinaciones, function(par) {
  test <- wilcox.test(par[[1]], par[[2]], paired = FALSE)  # Cambia a `TRUE` si los vectores están relacionados
  return(test$p.value)
})

# Mostrar los resultados del p-value para cada par
nombres_pares <- combn(nombres_vectores, 2, paste, collapse = " vs ")
names(resultados_wilcox) <- nombres_pares
print(resultados_wilcox)


#Crear un gráfico boxplot para comparar los vectores
datos <- data.frame(
  Ganancia = c(ML_1800, Ninguno_1800, EC_2200 , MICE_1600),
  Grupo = factor(rep(nombres_vectores, times = c(length(ML_1800), length(Ninguno_1800), length(EC_2200 ), length(MICE_1600))))
)

#library(ggplot2)
ggplot(datos, aes(x = Grupo, y = Ganancia, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparación de Ganancias Promedio entre Vectores", x = "Grupo", y = "Ganancia Promedio") +#  theme_minimal() +
  theme(legend.position = "none")



