# APLICANDO TEST DE WILCOX


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection


# Dados los siguientes vectores de ganancias promedio:

#vector de ganancias promedio obtenidas de aplicar Machine Learning (NA)
vector1 <- c(12030000, 12030000, 12270000, 12270000, 12390000, 12390000, 12270000, 12390000, 12150000, 12270000, 12750000, 12390000, 12390000, 12390000, 12150000, 12870000, 12510000, 12270000, 12870000, 12150000, 12510000, 12750000, 12750000, 11910000, 12270000, 12630000, 12150000, 12390000, 12510000, 12390000
) 

#vector de ganancias promedio obtenidas de aplicar "no hacer nada"
vector2 <- c(11424000,	11304000,	11784000,	11904000,	12144000,	11664000,	11664000,	11544000,	11544000,	11664000,	10944000,	11664000,	11544000,	11544000,	11664000,	11904000,	12144000,	11304000,	11664000,	11784000,	11544000,	11784000,	10944000,	11904000,	11064000,	11664000,	11784000,	11184000,	10944000,	10824000
)


#vector de ganancias promedio obtenidas de aplicar Estadistica Clasica
vector3 <- c(12108000, 11748000, 11748000, 12468000, 12468000, 11988000, 11988000, 12348000, 11988000, 12348000, 12708000,	11748000,	12228000,	12348000,	12228000,	12108000,	11988000,	12468000,	11748000,	12348000,	12228000,	12348000,	12348000,	12108000,	12228000,	12108000,	12468000,	12588000,	12228000,	12348000
) 


#vector de ganancias promedio obtenidas de aplicar MICE
vector4 <- c(11424000,	11304000,	11784000,	11904000,	12144000,	11664000,	11664000,	11544000,	11544000,	11664000,	10944000,	11664000,	11544000,	11544000,	11664000,	11904000,	12144000,	11304000,	11664000,	11784000,	11544000,	11784000,	10944000,	11904000,	11064000,	11664000,	11784000,	11184000,	10944000,	10824000
) 



#Realizar el test de Wilcoxon para cada par de vectores


combinaciones <- combn(list(vector1, vector2, vector3, vector4), 2, simplify = FALSE)
nombres_vectores <- c("vector1", "vector2", "vector3", "vector4")

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
  Ganancia = c(vector1, vector2, vector3, vector4),
  Grupo = factor(rep(nombres_vectores, times = c(length(vector1), length(vector2), length(vector3), length(vector4))))
)

library(ggplot2)
ggplot(datos, aes(x = Grupo, y = Ganancia, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparación de Ganancias Promedio entre Vectores", x = "Grupo", y = "Ganancia Promedio") +
  theme_minimal() +
  theme(legend.position = "none")


