wilcox.test(ganancias_log_base[1:100,7],ganancias_log_modificado[1:100,7], alternative = 'less')



library(ggplot2)

# Crear un data frame combinando ambos conjuntos de datos y agregando una columna para el tipo y el índice
data <- data.frame(
  valor = c(ganancias_log_base[1:100, 7], ganancias_log_modificado[1:100, 7]),
  tipo = rep(c("Base", "Modificado"), each = 100),
  indice = rep(1:100, times = 2)  # índice para el eje x
)

# Graficar
ggplot(data, aes(x = indice, y = valor, color = tipo)) +
  geom_point(size = 3) +
  labs(
    title = "Comparación de Ganancias Base y Modificadas",
    x = "Índice",
    y = "Valor de Ganancia",
    color = "Tipo de Ganancia"
  ) +
  scale_color_manual(values = c("Base" = "blue", "Modificado" = "red")) +
  theme_minimal()
